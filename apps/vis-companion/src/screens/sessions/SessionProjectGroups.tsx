/** One project's band, its session rows, and how that project is paged. */

import {
  memo,
  useCallback,
  useEffect,
  useLayoutEffect,
  useMemo,
  useRef,
  useState,
  type DragEvent,
  type MouseEvent,
  type ReactNode,
  type Ref,
} from 'react';

import {
  SessionRow,
  type SessionListActions,
  type SessionRowCommands,
  type SessionRowDeletion,
} from '../../components/SessionList';
import {
  HEADER_TRAIL,
  HeaderActions,
  HeaderTally,
  LIST_MARK,
  Pager,
  ProjectCrumb,
  ProjectStatusCounts,
  SectionHeader,
} from '../../components/SessionNavigator';
import { Menu, MenuBack, MenuItem, MenuNote, MENU_WIDTH } from '../../components/Menu';
import {
  ArchiveIcon,
  ChevronIcon,
  DotsIcon,
  NewSessionIcon,
  PaletteIcon,
  PencilIcon,
  ProjectsIcon,
  TrashIcon,
} from '../../components/icons';
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
  sessionIsArchived,
  sessionIsLive,
  sessionMillis,
  sessionNeedsInput,
  sessionRowKey,
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
import { hasHardwarePointer } from '../../lib/pointer';
import {
  groupFoldKey,
  projectFoldKey,
  projectRevealKey,
  readProjectFold,
  writeProjectFold,
} from '../../lib/project-fold';
import { SESSION_DRAG_MIME, useSessionDropTarget } from '../../lib/session-drag';
import type { ArchiveView, GatewayConn, Session, SessionGroup } from '../../lib/types';

/** Where inside the group sheet the reader is standing (`ProjectGroup`). */
type MenuStep =
  | { kind: 'root' }
  | { kind: 'sessions' }
  | { kind: 'new' }
  | { kind: 'group'; id: string }
  | { kind: 'rename'; id: string }
  | { kind: 'delete'; id: string }
  | { kind: 'colour'; id: string }
  | { kind: 'move'; sid: string };

/** One group as a band paints it: the gateway's row, or what a row itself said. */
type GroupBandView = {
  id: string;
  name: string;
  color: string | null;
  count: number;
  archived: boolean;
};

/**
 * The row to paint for one session: what the list holds, unless this band has just
 * re-filed it or put it away and the list's own window has not caught up yet
 * (`ProjectGroup`).
 */
function settled(
  session: Session,
  local: Map<string, Session>,
  refiled: ReadonlyMap<string, Session>,
): Session {
  const current = local.get(session.id) ?? session;
  const held = refiled.get(session.id);
  if (!held) return current;
  const moved =
    held.group_id !== current.group_id ||
    sessionIsArchived(held) !== sessionIsArchived(current);
  return moved ? held : current;
}

/** What a session is called in a list of choices. */
function rowTitle(session: Session): string {
  const title = typeof session.title === 'string' ? session.title.trim() : '';
  return title === '' ? 'Untitled session' : title;
}

/** Running rows, newest first regardless of favorite or draft ordering. */
function liveRuns(rows: Iterable<Session>): Session[] {
  return Array.from(rows)
    .filter((row) => sessionIsLive(row) && !sessionNeedsInput(row) && !sessionIsArchived(row))
    .sort((a, b) => sessionMillis(b) - sessionMillis(a));
}

/** A group's colour where no rail carries it: one mark, beside a group's name in a menu. */
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

/** Read a desktop batch, or the single id that existing mouse and touch drags carry. */
function droppedIds(event: DragEvent<HTMLElement>): string[] {
  const single = event.dataTransfer.getData('text/plain');
  const batch = event.dataTransfer.getData(SESSION_DRAG_MIME);
  // Legacy test carriers answer the same id for every format, unlike DataTransfer.
  if (!batch || batch === single) return single ? [single] : [];
  try {
    const ids: unknown = JSON.parse(batch);
    return Array.isArray(ids) && ids.length > 0 && ids.every((id) => typeof id === 'string' && id)
      ? ids
      : [];
  } catch {
    return [];
  }
}

/** A place that takes a dragged row from a browser pointer or the touch carrier. */
function useSessionDrop(onDropSession?: (sids: string[]) => void) {
  const [isOver, setIsOver] = useState(false);
  // A finger raises no drag events at all, so the touch carry (`lib/session-drag`)
  // reports the same hover and hands over the same session id: one place, and either
  // hand files a row into it.
  const carried = useSessionDropTarget(onDropSession ? (sid) => onDropSession([sid]) : undefined);
  return {
    isOver: (isOver || carried.isOver) && Boolean(onDropSession),
    dropProps: {
      ...carried.targetProps,
      onDragOver: (event: DragEvent<HTMLElement>) => {
        if (!onDropSession) return;
        event.preventDefault();
        event.dataTransfer.dropEffect = 'move';
        setIsOver(true);
      },
      onDragLeave: (event: DragEvent<HTMLElement>) => {
        const next = event.relatedTarget;
        if (next instanceof Node && event.currentTarget.contains(next)) return;
        setIsOver(false);
      },
      onDrop: (event: DragEvent<HTMLElement>) => {
        if (!onDropSession) return;
        event.preventDefault();
        setIsOver(false);
        const ids = droppedIds(event);
        if (ids.length > 0) onDropSession(ids);
      },
    },
  };
}

/** The band, rows and open space of one set all take the same drop. */
function SessionDropArea({
  onDropSession,
  areaRef,
  minHeight,
  children,
}: {
  onDropSession?: (sids: string[]) => void;
  areaRef?: Ref<HTMLDivElement>;
  minHeight?: number;
  children: ReactNode;
}) {
  const { isOver, dropProps } = useSessionDrop(onDropSession);
  return (
    <div
      ref={areaRef}
      style={{ minHeight }}
      className={isOver ? 'bg-white/10' : undefined}
      {...dropProps}
    >
      {children}
    </div>
  );
}

/**
 * The name of ONE SET inside a project: the groups a reader filed, and the sessions
 * that are in none of them. A filed session stands under its band and NOWHERE else, so
 * without these two words the shelves and the page under them read as a single list
 * that happens to wear a coloured stripe on some of its rows.
 */
function SetHeader({
  label,
  navigation,
  action,
  isArchived = false,
}: {
  label: 'Groups' | 'Sessions';
  /**
   * THIS SET'S OWN STEPS, on the trailing edge of its band. The bands and the loose
   * sessions under them are two lists cut apart, so each set carries the pager that
   * moves it: one pair of arrows in the project header above them moved whichever of
   * the two the reader was not looking at.
   */
  navigation?: ReactNode;
  /** The menu owned by this set, after its page controls. */
  action?: ReactNode;
  /** Whether this set shows its archived entries instead of active ones. */
  isArchived?: boolean;
}) {
  return (
    <div
      className={`flex min-h-14 flex-wrap items-center gap-x-2 gap-y-0 border-y border-edge-strong py-1 pl-4 max-sm:sticky max-sm:top-13 max-sm:z-5 mouse:min-h-10 ${label === 'Groups' ? 'bg-set-groups' : 'bg-set-sessions'}`}
    >
      <span
        className={`font-mono text-ui font-bold tracking-[0.08em] uppercase ${label === 'Groups' ? 'text-accent-ink' : 'text-white'}`}
      >
        {label}
      </span>
      {isArchived && <span className="font-mono text-meta text-white">Archived</span>}
      {/* The set menu follows its own page controls on the trailing edge. */}
      {(action || navigation) && (
        <span className={`ml-auto ${HEADER_TRAIL}`}>
          {navigation}
          {action}
        </span>
      )}
    </div>
  );
}

/**
 * One group's own band, inside its project's list.
 *
 * A level quieter than the project header above it, and deliberately so: the project owns
 * the boundary rule, while a group owns its name, its colour and its own fold. A band is
 * never paged from the INSIDE — it is a shelf a reader reads whole — but the WALL of bands
 * is paged, and those steps stand on the `Groups` header over them.
 */
function GroupBand({
  name,
  color,
  isOpen,
  onToggle,
  onActions,
}: {
  name: string;
  color: string | null;
  isOpen: boolean;
  onToggle: () => void;
  onActions: (anchor: HTMLElement) => void;
}) {
  return (
    <div className="flex items-stretch border-t border-edge">
      {/* The band and its rows share one coloured edge, so a group reads as a place
          rather than as a caption. That rail is the ONLY place this colour is painted
          in the list: a dot beside the name repeated what the edge already says. */}
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
        <span className="min-w-0 truncate font-mono text-ui font-bold text-white">{name}</span>
      </button>
      <HeaderActions align="center">
        <IconButton
          label={`Actions for ${name}`}
          variant="quiet"
          aria-haspopup="dialog"
          onClick={(event) => onActions(event.currentTarget)}
        >
          <DotsIcon className="size-3.5" />
        </IconButton>
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
  /**
   * The session the pane beside this list is showing, as `sessionRowKey` names it —
   * null while nothing is open. A STRING, never the connection: this context is
   * memoised, and a fresh object per paint re-renders every row of a long list.
   */
  openRow: string | null;
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

/**
 * How many of a project's GROUPS stand on one page of them.
 *
 * A band is not a row: it carries its own name, its count and the shelf of sessions
 * under it, so ten of them is already a screen. This step is therefore the wall's own,
 * not the one the screen measured for rows (`useSessionsPerPage`).
 */
const GROUPS_PAGE = 10;

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
  const { getClient, drafts, matches, needle, actions: rowActions, openRow } = context;
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
  // THE WALL OF BANDS IS PAGED TOO, and by a NUMBER rather than a cursor: the human's own
  // `position` orders the groups and no turn moves one, so the third page of them names
  // the same shelves tomorrow (`state/list-session-groups-page`).
  const [groupPage, setGroupPage] = useState(1);
  // THE WALL'S OWN TALLIES, as the gateway counted them: how many bands this project
  // holds, and how many sessions are filed across ALL of them. Both count the wall and
  // not the page of it on screen, so the steps over the bands and the counts beside them
  // do not move while the reader walks it.
  const [groupTotal, setGroupTotal] = useState(0);
  const [groupSessionTotal, setGroupSessionTotal] = useState(0);
  // ONE WINDOW OVER THE WALL, ASKED FOR BY BOTH READS. The bands on this page and the
  // sessions filed under them are two halves of one answer, so the session read carries
  // the same window: `grouped` then holds those bands' rows and nothing from a shelf that
  // is off the page (`listProjectPage`).
  const bandWindow = useMemo(
    () => ({ limit: GROUPS_PAGE, offset: (groupPage - 1) * GROUPS_PAGE }),
    [groupPage],
  );
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
    // Only the active session page is available in the held window.
    if (readProjectFold(projectRevealKey(machineKey(conn), root, 'sessions'))) return null;
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
  // Each set remembers its own archive view. A search shows every matching row without
  // changing either saved choice; clearing it restores both views.
  const groupRevealKey = projectRevealKey(machineKey(conn), root, 'groups');
  const sessionRevealKey = projectRevealKey(machineKey(conn), root, 'sessions');
  const [isGroupRevealed, setIsGroupRevealed] = useState(
    () => readProjectFold(groupRevealKey) ?? false,
  );
  const [isSessionRevealed, setIsSessionRevealed] = useState(
    () => readProjectFold(sessionRevealKey) ?? false,
  );
  const isGroupRevealing = isGroupRevealed && needle === '';
  const isSessionRevealing = isSessionRevealed && needle === '';
  const archived: ArchiveView = isSessionRevealing ? 'only' : 'exclude';
  const groupArchived: ArchiveView = isGroupRevealing ? 'only' : 'exclude';
  const [groupsRead, setGroupsRead] = useState(0);
  const [groupsReady, setGroupsReady] = useState<{ view: ArchiveView; offset: number } | null>(
    null,
  );
  // The gateway leaves the sessions unstamped when it archives their group. Hold its
  // answer until both the next page and group reads arrive, then defer to fresh reads.
  const [groupArchive, setGroupArchive] = useState<{
    id: string;
    archived: boolean;
    refresh: number;
  } | null>(null);
  const refreshed = useRef({ page: -1, groups: -1 });
  const noteRefresh = useCallback((part: 'page' | 'groups', read: number) => {
    refreshed.current[part] = read;
    if (refreshed.current.page === read && refreshed.current.groups === read)
      setGroupArchive((held) => (held && held.refresh <= read ? null : held));
  }, []);
  const revealGroups = (on: boolean) => {
    writeProjectFold(groupRevealKey, on);
    setIsGroupRevealed(on);
    // The group wall owns its page; leave the loose session cursor and page alone.
    setGroupPage(1);
    // If the main page now supplies the grouped sidecar, revalidate it: a group
    // may have moved between views since that loose-session page was last read.
    if ((on ? 'only' : 'exclude') === archived) setGroupsRead((read) => read + 1);
    fold(true);
  };
  const revealSessions = (on: boolean) => {
    writeProjectFold(sessionRevealKey, on);
    setIsSessionRevealed(on);
    // Cursors and the held page name this session view, not the other one.
    cursors.current = new Map([[0, '']]);
    asked.current = '';
    setPaged(null);
    setFirst(0);
    fold(true);
  };
  // Even an empty project can have archived groups or start a new session.
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
  const isShowing = isOpen || needle !== '';
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
    // The window over the BANDS is part of that question: the shelves are the other half
    // of this answer, so turning their page repaints from the page held for it.
    const question = `${limit}\u0000${after}\u0000${bandWindow.offset}`;
    if (question !== asked.current) {
      asked.current = question;
      const held = api.heldProjectPage(root, limit, after, pins.current, archived, bandWindow);
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
          archived,
          bandWindow,
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
        noteRefresh('page', groupsRead);
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
            false,
            archived,
            bandWindow,
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
  }, [
    conn, root, start, pageSize, isVisible, isShowing, searching, archived, bandWindow, list,
    getClient, groupsRead, noteRefresh,
  ]);
  // The gateway applies one archive filter to both sides of a project page. When the
  // two sets differ, read the grouped sidecar in its own view; the limit of one only
  // applies to loose sessions, while grouped rows arrive complete for the band window.
  const [groupedPage, setGroupedPage] = useState<{
    view: ArchiveView;
    offset: number;
    rows: Session[];
  } | null>(null);
  useEffect(() => {
    if (!isVisible || !isShowing || searching || groupArchived === archived) return;
    const control = new AbortController();
    let live = true;
    void getClient(conn)
      .listProjectPage(root, 1, '', pins.current, control.signal, false, groupArchived, bandWindow)
      .then((answer) => {
        if (!live) return;
        setGroupedPage({ view: groupArchived, offset: bandWindow.offset, rows: answer.grouped });
        noteRefresh('page', groupsRead);
      })
      .catch(() => {
        // Keep the last matching view if the machine is temporarily unavailable.
      });
    return () => {
      live = false;
      control.abort();
    };
  }, [
    conn, root, isVisible, isShowing, searching, groupArchived, archived, bandWindow, list,
    getClient, groupsRead, noteRefresh,
  ]);
  // The count under the header and the pages beside it are ONE number — the
  // project's own total, as the gateway counted it. Under a query the complete
  // answer is on this device, and then what is on screen is the honest count.
  const total = searching ? sessions.length : (paged?.total ?? tally.count);
  const pageCount = Math.max(1, Math.ceil(Math.max(total, 1) / pageSize));
  const shownPage = searching
    ? Math.min(page, pageCount)
    : Math.min(paged ? Math.floor(paged.start / pageSize) + 1 : 1, pageCount);
  // The bands are paged out of the wall's own total, the same way: the steps over them
  // print how many pages of GROUPS this project has, not how many are on screen.
  const groupPageCount = Math.max(1, Math.ceil(Math.max(groupTotal, 1) / GROUPS_PAGE));
  const shownGroupPage = Math.min(groupPage, groupPageCount);
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
  // AND THE MACHINE'S HEAD WINDOW IS THE ACTIVE LIST. A reveal has only its own read to
  // paint from, so it waits that read out rather than opening on the rows it is not about.
  const headFills =
    !isSessionRevealing && start === 0 && held.length >= Math.min(pageSize, tally.count);
  const painting = searching
    ? sessions.slice((shownPage - 1) * pageSize, shownPage * pageSize)
    : (pageRows ?? (headFills ? held : NO_ROWS));
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
  const [groups, setGroups] = useState<SessionGroup[]>([]);
  const archivedGroupIds = useMemo(
    () => new Set(groups.filter((group) => group.archived_at != null).map((group) => group.id)),
    [groups],
  );
  const paints = useCallback(
    (session: Session) => {
      if (searching) return true;
      const gid = typeof session.group_id === 'string' ? session.group_id : '';
      if (!gid) return sessionIsArchived(session) === isSessionRevealing;
      const archivedByGroup =
        groupArchive && groupArchive.id === gid
          ? groupArchive.archived
          : archivedGroupIds.has(gid);
      return (sessionIsArchived(session) || archivedByGroup) === isGroupRevealing;
    },
    [searching, groupArchive, archivedGroupIds, isGroupRevealing, isSessionRevealing],
  );
  // NOTHING MOVES WHILE THE READER IS LOOKING AT IT (`lib/order-epoch`). The list
  // of projects is held by the screen; a page read from the gateway is held HERE,
  // or a turn finishing on another machine would slide this page under the thumb on
  // the next poll. A search answer arrives held already, and a row this reader
  // started or is holding words for is admitted rather than parked behind the pill.
  const rows = useMemo(() => {
    const api = getClient(conn);
    const shown = painting
      .filter((session) => !api.isSessionDeleted(session.id))
      .map((session) => settled(session, local, refiled))
      .filter(paints);
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
      .map((session) => settled(session, local, refiled))
      .filter(paints);
    return parked.length === 0 ? held : [...parked, ...held];
  }, [searching, painting, local, refiled, paints, epoch, admitted, paged, getClient, conn, list]);
  // A BAND IS NEVER CUT BY THE SESSION PAGE. The gateway answers the FILED sessions of
  // the bands on this page complete and beside the window (`?grouped=aside`), because a
  // group is a shelf a reader reads whole: bands cut from the current page printed a name
  // with `none on this page` under it while its sessions sat four pages down, and a
  // session filed from the sheet left the very band that had just taken it. What IS paged
  // is the wall of bands itself, and a shelf off that page is not painted here at all
  // (`&group_limit=`).
  const shelved = useMemo(() => {
    const api = getClient(conn);
    // A query is answered over the whole project already; its hits are the list.
    if (searching) return NO_ROWS;
    const grouped =
      groupArchived === archived
        ? paged?.grouped
        : groupedPage?.view === groupArchived && groupedPage.offset === bandWindow.offset
          ? groupedPage.rows
          : NO_ROWS;
    return (grouped ?? NO_ROWS)
      .filter((session) => !api.isSessionDeleted(session.id))
      .map((session) => settled(session, local, refiled))
      .filter(paints);
  }, [
    searching, paged, groupedPage, groupArchived, archived, bandWindow, local, refiled, paints,
    getClient, conn,
  ]);
  // Every row this project is painting: the shelves, and the page under them. A verb
  // aimed at a row - a drop, a `Move to...` - has to find it wherever it stands.
  const painted = useMemo(() => [...shelved, ...rows], [shelved, rows]);
  useEffect(() => {
    // The project shrank under the pager (a deletion, a smaller step): the page that
    // no longer exists becomes the first one rather than the last one a reader never
    // asked for.
    if (page > pageCount) setFirst(0);
  }, [page, pageCount]);
  useEffect(() => {
    // The wall shrank under its own steps (a band deleted, a band put away): the same
    // rule as the page above, on the other list.
    if (groupPage > groupPageCount) setGroupPage(1);
  }, [groupPage, groupPageCount]);
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
  // exists here is a band every client of the machine paints. Read as soon as the
  // project is on screen, so a project that opens paints its bands already named.
  // The gateway owns the groups and their archive stamps, not this device.
  useEffect(() => {
    if (!isVisible) return;
    const control = new AbortController();
    let live = true;
    void (async () => {
      try {
        const answer = await getClient(conn).listSessionGroups(
          root,
          control.signal,
          groupArchived,
          bandWindow,
        );
        if (!live) return;
        setGroups(answer?.groups ?? []);
        setGroupTotal(answer?.total ?? 0);
        setGroupSessionTotal(answer?.session_total ?? 0);
        setGroupsReady({ view: groupArchived, offset: bandWindow.offset });
        noteRefresh('groups', groupsRead);
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
  }, [conn, root, isVisible, groupArchived, bandWindow, getClient, groupsRead, noteRefresh]);
  // WHERE EACH ROW IS FILED, AND WHICH OF THE TWO SETS IT IS IN. A group's rows stay
  // CONTIGUOUS under its own name, and a filed row is NOT in the list below the bands —
  // which keeps the project's own order. It is the shape the TUI's navigator paints
  // (`tui/dialogs`), so one list is not two different pictures.
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
    const known = groups
      .map((group) => ({
        id: group.id,
        name: group.name,
        color: group.color,
        count: group.session_count,
        archived: groupArchive?.id === group.id ? groupArchive.archived : group.archived_at != null,
      }))
      .filter((group) => group.archived === isGroupRevealing);
    // A REVEAL PAINTS THE ARCHIVE'S OWN BANDS AND NOTHING ELSE. A session archived inside
    // a group that is still active belongs to THAT group's reveal; a band invented here
    // from the group such a row names would file it under a project that does not have it.
    if (isGroupRevealing) return known;
    const unread = [...filed.byGroup.entries()]
      .filter(([gid]) => !groups.some((group) => group.id === gid) && groupArchive?.id !== gid)
      .map(([gid, held]) => ({
        id: gid,
        // The GROUP owns its name and its colour, and this device has not read THIS one
        // yet. The band stands under a plain word until the re-read below lands it.
        name: 'Group',
        color: null,
        count: held.length,
        archived: false,
      }));
    return [...known, ...unread];
  }, [groups, filed, isGroupRevealing, groupArchive]);
  // Both sides of the project header count the sets currently on display.
  const visibleCount = total + groupSessionTotal;
  // A ROW WEARS ITS GROUP'S COLOUR, NEVER A COPY OF IT. The rail down a row and the band
  // over it read the SAME group, so a recolour lands on both in the same paint.
  // Reported in this Vis session with a screenshot (paraphrased: choosing a colour changed
  // some of the rows and left the others alone) — the rows were painting a copy stamped on
  // them when the gateway read them, which a later recolour could not reach.
  const groupById = useMemo(() => new Map(groups.map((group) => [group.id, group])), [groups]);
  // A ROW CAN NAME A GROUP THIS DEVICE HAS NOT READ: it was made on another client, or the
  // rows landed a beat ahead of the groups. Ask for the groups again ONCE per such id, so
  // that band gets its own name and colour instead of standing as a plain "Group" forever.
  const askedForGroup = useRef(new Set<string>());
  useEffect(() => {
    let missing = false;
    for (const gid of filed.byGroup.keys()) {
      if (groupById.has(gid) || askedForGroup.current.has(gid)) continue;
      askedForGroup.current.add(gid);
      missing = true;
    }
    if (missing) setGroupsRead((read) => read + 1);
  }, [filed, groupById]);
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
  // A range belongs to what this project actually paints, not to rows on another page
  // or a folded shelf. Keep its anchor even though the first plain click opens a session.
  const listed = searching ? painted : filed.loose;
  const visibleIds = searching
    ? listed.map((session) => session.id)
    : [
        ...bands.flatMap((band) =>
          isGroupOpen(band.id) ? (filed.byGroup.get(band.id) ?? NO_ROWS).map((row) => row.id) : [],
        ),
        ...listed.map((session) => session.id),
      ];
  const selectionScope = JSON.stringify([
    page, groupPage, pageSize, needle, archived, groupArchived, isShowing, groupFolds,
  ]);
  const anchor = useRef<{ id: string; scope: string } | null>(null);
  const [selection, setSelection] = useState<{ scope: string; ids: string[] } | null>(null);
  const visibleSet = new Set(visibleIds);
  const selectedIds = selection?.scope === selectionScope
    ? selection.ids.filter((id) => visibleSet.has(id))
    : [];
  const selectedSet = new Set(selectedIds);
  useEffect(() => {
    if (selectedIds.length === 0) return;
    const onKey = (event: KeyboardEvent) => {
      if (event.key !== 'Escape') return;
      setSelection(null);
      anchor.current = null;
    };
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [selectedIds.length]);
  const onSelectionClick = (id: string, event: MouseEvent<HTMLButtonElement>): boolean => {
    if (!event.shiftKey || !hasHardwarePointer()) {
      anchor.current = { id, scope: selectionScope };
      setSelection(null);
      return false;
    }
    event.preventDefault();
    const start = anchor.current?.scope === selectionScope
      ? visibleIds.indexOf(anchor.current.id)
      : -1;
    const end = visibleIds.indexOf(id);
    if (end < 0) return true;
    const from = start < 0 ? end : start;
    if (start < 0) anchor.current = { id, scope: selectionScope };
    setSelection({ scope: selectionScope, ids: visibleIds.slice(Math.min(from, end), Math.max(from, end) + 1) });
    return true;
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
  // A refused action stays in its sheet. A name collision and a busy member are
  // different 409s: archiving the whole shelf must explain why it stayed put.
  const attempt = async (act: () => Promise<unknown>, next: MenuStep | 'close') => {
    setIsBusy(true);
    setFailure(null);
    try {
      await act();
      setGroupsRead((count) => count + 1);
      if (next === 'close') setMenu(null);
      else goTo(next);
    } catch (failed) {
      const errorType =
        failed instanceof GatewayError
          ? (failed.body as { error?: { type?: string } } | null)?.error?.type
          : null;
      setFailure(
        errorType === 'session-busy'
          ? 'A session in this group is still active. Archive it once its turn is done.'
          : failed instanceof GatewayError && failed.status === 409
            ? 'This project already has a group with that name.'
            : 'That did not reach the machine. Try again.',
      );
    } finally {
      setIsBusy(false);
    }
  };
  // FILING A SESSION IS THE GATEWAY'S ANSWER, held here until the list catches up. The
  // call stands apart from the sheet that usually runs it, so a row's own verb can file
  // with no menu open and the list still holds what the machine answered.
  const assignGroup = useCallback(
    async (session: Session, gid: string | null) => {
      const moved = await getClient(conn).assignSessionGroup(session.id, gid);
      setRefiled((held) => new Map(held).set(moved.id, moved));
    },
    [conn, getClient],
  );
  // PUTTING A ROW AWAY RIDES THE SAME BRIDGE FILING DOES: the screen's verb answers with the
  // row the gateway stamped, and it is held here until the list's window catches up, so the
  // band it left stops painting it without waiting for a poll.
  const archiveSession = useMemo(() => {
    const putAway = rowActions.commands.archive;
    if (!putAway) return undefined;
    return async (session: Session, rowConn: GatewayConn, away: boolean) => {
      const moved = await putAway(session, rowConn, away);
      setRefiled((held) => new Map(held).set(moved.id, moved));
      return moved;
    };
  }, [rowActions.commands.archive]);
  const fileSession = (session: Session, gid: string | null, back: MenuStep | 'close') =>
    void attempt(() => assignGroup(session, gid), back);
  // One session's row, wherever it stands: inside a group's area, or under the
  // project itself with everything nobody filed. A drop onto either area runs the
  // same filing verb as the row's `Move to...` action.
  const [dropFailure, setDropFailure] = useState<string | null>(null);
  const dropping = useRef(false);
  const dropSessions = async (ids: string[], gid: string | null) => {
    if (dropping.current) return;
    const known = new Map(painted.map((session) => [session.id, session]));
    // A project takes its own visible sessions, never foreign ids or rows from a stale page.
    if (ids.some((id) => !known.has(id))) return;
    const unique = [...new Set(ids)];
    const moving = unique
      .map((id) => known.get(id)!)
      .filter((session) => (session.group_id ?? null) !== gid);
    if (moving.length === 0) return;
    dropping.current = true;
    setDropFailure(null);
    const completed = new Set(unique.filter((id) => (known.get(id)!.group_id ?? null) === gid));
    let failed = 0;
    try {
      for (const session of moving) {
        try {
          await assignGroup(session, gid);
          completed.add(session.id);
        } catch {
          failed += 1;
        }
      }
      if (completed.size > 0) {
        setSelection((held) => {
          if (!held || held.scope !== selectionScope) return held;
          const remaining = held.ids.filter((id) => !completed.has(id));
          return remaining.length > 0 ? { ...held, ids: remaining } : null;
        });
      }
      if (completed.size > unique.length - moving.length) setGroupsRead((read) => read + 1);
      if (failed > 0)
        setDropFailure(`${failed} of ${moving.length} sessions could not be moved. Try again.`);
    } finally {
      dropping.current = false;
    }
  };
  // Every row in these bands can be filed, so the list's own verbs carry one more: the
  // sheet opens under the strip's button, on the session that strip belongs to.
  const rowCommands = useMemo<SessionRowCommands>(
    () => ({
      ...rowActions.commands,
      archive: archiveSession,
      moveToGroup: (session, _conn, anchor) => openMenu(anchor, { kind: 'move', sid: session.id }),
    }),
    [archiveSession, openMenu, rowActions.commands],
  );
  // THE ONLY GROUP THIS PROJECT HAS IS THE ONE THE ROW IS ALREADY UNDER, so the sheet
  // would offer that band and nothing else. The strip carries the one filing that is
  // left instead, and runs it in the press that was made. A machine that refuses leaves
  // the row where it stands, exactly as a refused drop onto a band does.
  const ungroupSession = useCallback(
    (session: Session) =>
      void (async () => {
        try {
          await assignGroup(session, null);
          setGroupsRead((read) => read + 1);
        } catch {
          // No sheet is open to carry a note, and the row staying put is the answer.
        }
      })(),
    [assignGroup],
  );
  const soleGroupCommands = useMemo<SessionRowCommands>(
    () => ({ ...rowActions.commands, archive: archiveSession, ungroup: ungroupSession }),
    [archiveSession, rowActions.commands, ungroupSession],
  );
  const row = (session: Session) => {
    const pending = pendingDeleteId === session.id;
    // Nowhere to move it: this project has ONE group and the row is filed under it.
    const soleGroup = bands.length === 1 && bands[0].id === session.group_id;
    const deletion: SessionRowDeletion = pending
      ? {
          isBusy: rowActions.deletion.isBusy,
          error: rowActions.deletion.error,
          confirm: rowActions.deletion.confirm,
          cancel: rowActions.deletion.cancel,
        }
      : null;
    return (
      // DRAG IS THE LIST'S OWN MOVE: the row carries its session id and a group's
      // area takes the drop, so filing by hand needs no menu at all.
      <SessionRow
        key={session.id}
        session={session}
        group={groupById.get(typeof session.group_id === 'string' ? session.group_id : '') ?? null}
        draft={drafts[draftMessageKey(base, session.id)] ?? EMPTY_DRAFT_MESSAGE}
        conn={conn}
        match={matches?.get(session.id) ?? null}
        needle={needle}
        commands={soleGroup ? soleGroupCommands : rowCommands}
        deletion={deletion}
        isOpen={openRow !== null && openRow === sessionRowKey(conn, session.id)}
        isSelected={selectedSet.has(session.id)}
        onSelectionClick={(event) => onSelectionClick(session.id, event)}
        dragIds={selectedSet.has(session.id) ? selectedIds : undefined}
        isDraggable={session.group_id ? !isGroupRevealing : !isSessionRevealing}
      />
    );
  };

  const sessionSetRef = useRef<HTMLDivElement>(null);
  const pageLayout = `${pageSize}\u0000${archived}\u0000${needle}`;
  const [pageFootprint, setPageFootprint] = useState<{ layout: string; height: number } | null>(
    null,
  );
  const goToPage = (next: number) => {
    // A shorter page must not shrink the scroller and push the reader back into Groups.
    // Reserve this set's outgoing height, not the groups above it. A different page size,
    // search or archive view starts with its own natural height.
    const set = sessionSetRef.current;
    if (set) setPageFootprint({ layout: pageLayout, height: set.getBoundingClientRect().height });
    setFirst((next - 1) * pageSize);
  };
  const pager =
    pageCount > 1 ? (
      <Pager
        page={shownPage}
        pageCount={pageCount}
        onPage={goToPage}
        label={`${project} sessions`}
      />
    ) : null;

  // The bands' own steps, over the set they move. A project whose wall fits on one page
  // shows none of this (`Pager`).
  const groupPager =
    groupPageCount > 1 ? (
      <Pager
        page={shownGroupPage}
        pageCount={groupPageCount}
        onPage={setGroupPage}
        label={`${project} groups`}
      />
    ) : null;
  // Use the same open command as a row. The gateway's count covers the whole project,
  // so a run outside the loaded window must remain reachable from a collapsed band.
  const running = Math.max(0, tally.live - (tally.awaiting ?? 0));
  const [isOpeningLive, setIsOpeningLive] = useState(false);
  const [liveFailure, setLiveFailure] = useState<string | null>(null);
  const liveRequest = useRef<AbortController | null>(null);
  useEffect(
    () => () => liveRequest.current?.abort(),
    [conn, root, isVisible, isGroupRevealing, isSessionRevealing],
  );
  const openLive = async () => {
    if (liveRequest.current) return;
    setLiveFailure(null);
    const held = liveRuns(sessions);
    if (held.length >= running && held[0]) {
      void rowActions.commands.open(conn, held[0].id);
      return;
    }

    const control = new AbortController();
    liveRequest.current = control;
    setIsOpeningLive(true);
    try {
      const api = getClient(conn);
      const windows: ProjectWindows = new Map();
      const found = new Map<string, Session>();
      const visited = new Set<string>();
      let after = '';
      do {
        visited.add(after);
        const answer = await api.listProjectPage(root, 100, after, windows, control.signal);
        if (control.signal.aborted) return;
        // Without a band window, the sidecar includes every group's rows too.
        for (const row of liveRuns([...answer.rows, ...answer.grouped])) found.set(row.id, row);
        after = answer.nextCursor;
      } while (found.size < running && after && !visited.has(after));
      const target = liveRuns(found.values())[0];
      if (target) void rowActions.commands.open(conn, target.id);
      else setLiveFailure('No sessions are running in this project now.');
    } catch {
      if (!control.signal.aborted) setLiveFailure('Could not open the live session. Try again.');
    } finally {
      if (liveRequest.current === control) {
        liveRequest.current = null;
        setIsOpeningLive(false);
      }
    }
  };

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
        arrival the reader cannot reach; the total ellipsises instead and keeps it whole.

        AN ARRIVAL STANDS AFTER THE STATES IT INTERRUPTS. Reported over this band
        (paraphrased: the count of sessions that just landed belongs to the RIGHT of
        live, not its left): the caption read `1662 sessions | 2 new · 4 live`, so the
        one thing on the line that is a VERB sat between the total and the states that
        qualify it, and the reader's eye had to cross it to reach `4 live`. */}
      <span className="flex min-w-0 items-center">
        {/* THE TOTAL AND THE STATES ARE ONE RUN OF TEXT, and what can TAP stands beside
            that run. `text-overflow` elides TEXT and drops an atomic box whole, so this
            is what lets the smallest phone shorten `1464 sessions · 2 needs input` and
            still hold the whole of the live count and the arrival. */}
        <span className="min-w-0 truncate">
          {/* The two archive choices can differ; the count describes the two sets in view. */}
          <HeaderTally
            count={isGroupRevealing || isSessionRevealing ? visibleCount : tally.count}
            unit="session"
          />
          {isGroupRevealing || isSessionRevealing ? (
            <>
              <span aria-hidden className="mx-2 @max-md:mx-1">·</span>
              <span className="whitespace-nowrap font-bold">
                {isGroupRevealing && isSessionRevealing ? 'Archived' : 'Mixed views'}
              </span>
            </>
          ) : (
            <ProjectStatusCounts live={0} awaiting={tally.awaiting} unread={tally.unread} />
          )}
        </span>
        {!isGroupRevealing && !isSessionRevealing && running > 0 && (
          <>
            <span aria-hidden className="mx-2 shrink-0 @max-md:mx-1">
              ·
            </span>
            <TextButton
              isCaption
              className="pointer-events-auto relative shrink-0 whitespace-nowrap font-bold"
              aria-label={
                running === 1
                  ? 'Open the live session'
                  : `Open the newest of ${running} live sessions`
              }
              disabled={isOpeningLive}
              aria-busy={isOpeningLive}
              onClick={() => void openLive()}
            >
              <span
                aria-hidden="true"
                className="mr-1 inline-block size-1.5 animate-pulse bg-ok align-[0.05em] motion-reduce:animate-none"
              />
              {running} live
            </TextButton>
          </>
        )}
        {hasPending && (
          <>
            <span aria-hidden className="shrink-0 whitespace-pre"> | </span>
            {/* The arrival takes the caption's step, so the line stays one run of type and
                its bottom stays on the total's (`AcceptNewerSession` measures it). */}
            <TextButton
              isCaption
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
      </span>
    </span>
  );

  // The project paints two sets: groups and loose sessions. Each keeps its own
  // actions on its own header, even when empty. A query shows one session set.
  const hasGroups = bands.length > 0;
  // `listed` was computed above so selection and the rendered list share one order.
  // Each archive answers for its own set, including when the other set has rows.
  const emptyGroups =
    isGroupRevealing &&
    groupsReady?.view === groupArchived &&
    groupsReady.offset === bandWindow.offset &&
    groupTotal === 0;
  const emptySessions = isSessionRevealing && paged !== null && listed.length === 0;
  const paintsSets = isShowing;

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
        <SectionHeader isExpanded={isShowing}>
          <ProjectCrumb
            name={project}
            qualifier={qualifier}
            qualifierTitle={root}
            disclosure={{
              isOpen: isShowing,
              onToggle: () => fold(!isShowing),
              label: `${isShowing ? 'Collapse' : 'Expand'} ${project}`,
            }}
          />
        </SectionHeader>
        {liveFailure && !isGroupRevealing && !isSessionRevealing && (
          <p
            role="alert"
            className="border-b border-edge px-4 py-3 font-mono text-meta text-dialog-hint"
          >
            {liveFailure}
          </p>
        )}
        {dropFailure && (
          <p role="alert" className="border-b border-edge px-4 py-3 font-mono text-meta text-danger">
            {dropFailure}
          </p>
        )}
        {/* Rows own their internal dividers; the wrapper closes the final session. */}
        {paintsSets && (
          <div
            ref={rowsRef}
            className={`border-b ${needle ? 'border-dialog-hint' : 'border-edge'}`}
          >
            {!searching && (
              <div>
                <SetHeader
                  label="Groups"
                  isArchived={isGroupRevealing}
                  navigation={groupPager}
                  action={
                    <IconButton
                      label={`Actions for groups in ${root}`}
                      variant="quiet"
                      aria-haspopup="dialog"
                      aria-expanded={menu?.step.kind === 'root'}
                      onClick={(event) => openMenu(event.currentTarget, { kind: 'root' })}
                    >
                      <DotsIcon className="size-3.5" />
                    </IconButton>
                  }
                />
                {emptyGroups && (
                  <p className="px-4 py-3 font-mono text-meta text-dialog-hint">
                    No archived groups in this project.
                  </p>
                )}
                {bands.map((band) => {
                  const held = filed.byGroup.get(band.id) ?? NO_ROWS;
                  const isBandOpen = isGroupOpen(band.id);
                  return (
                    <SessionDropArea
                      key={band.id}
                      onDropSession={isGroupRevealing ? undefined : (ids) => void dropSessions(ids, band.id)}
                    >
                      <GroupBand
                        name={band.name}
                        color={band.color}
                        isOpen={isBandOpen}
                        onToggle={() => foldGroup(band.id, !isBandOpen)}
                        onActions={(anchor) => openMenu(anchor, { kind: 'group', id: band.id })}
                      />
                      {isBandOpen && held.map(row)}
                    </SessionDropArea>
                  );
                })}
              </div>
            )}
            <SessionDropArea
              areaRef={sessionSetRef}
              minHeight={
                pageCount > 1 && pageFootprint?.layout === pageLayout
                  ? pageFootprint.height
                  : undefined
              }
              onDropSession={
                hasGroups && !isSessionRevealing ? (ids) => void dropSessions(ids, null) : undefined
              }
            >
              {/* A band files into itself; this area takes a session back out. */}
              <SetHeader
                label="Sessions"
                isArchived={isSessionRevealing}
                navigation={pager}
                action={
                  <IconButton
                    label={`Actions for sessions in ${root}`}
                    variant="quiet"
                    aria-haspopup="dialog"
                    aria-expanded={menu?.step.kind === 'sessions'}
                    onClick={(event) => openMenu(event.currentTarget, { kind: 'sessions' })}
                  >
                    <DotsIcon className="size-3.5" />
                  </IconButton>
                }
              />
              {listed.map(row)}
              {emptySessions && (
                <p className="px-4 py-3 font-mono text-meta text-dialog-hint">
                  No archived sessions in this project.
                </p>
              )}
            </SessionDropArea>
          </div>
        )}
      </section>
      {menu && (
        <Menu
          label={`${menu.step.kind === 'sessions' ? 'Sessions' : 'Groups'} in ${project}`}
          at={menu.at}
          onDismiss={() => setMenu(null)}
        >
          {(() => {
            const step = menu.step;
            // Each set opens its own actions; group rows keep their own verbs.
            if (step.kind === 'root')
              return (
                <>
                  <MenuItem
                    title="New group"
                    icon={<ProjectsIcon className="size-3.5" />}
                    onSelect={() => {
                      setTyped('');
                      goTo({ kind: 'new' });
                    }}
                  />
                  <MenuItem
                    title={isGroupRevealed ? 'Hide archived groups' : 'Show archived groups'}
                    icon={<ArchiveIcon className="size-3.5" />}
                    onSelect={() => {
                      revealGroups(!isGroupRevealed);
                      setMenu(null);
                    }}
                  />
                  {failure && <MenuNote>{failure}</MenuNote>}
                </>
              );
            if (step.kind === 'sessions')
              return (
                <>
                  <MenuItem
                    title="New session"
                     icon={<NewSessionIcon className="size-3.5" />}
                    disabled={creating?.at === creationKey(base, root)}
                    onSelect={() => {
                      setMenu(null);
                      void onNewSession(conn, root);
                    }}
                  />
                  <MenuItem
                    title={isSessionRevealed ? 'Hide archived sessions' : 'Show archived sessions'}
                    icon={<ArchiveIcon className="size-3.5" />}
                    onSelect={() => {
                      revealSessions(!isSessionRevealed);
                      setMenu(null);
                    }}
                  />
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
                      icon={<ProjectsIcon className="size-3.5" />}
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
            const bandArchived = band.archived;
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
            // others, so both answers are offered and the destructive one is second, in
            // the app's red behind the bin. Like the verbs this step was chosen from, it
            // wears no band spelling the name the reader just pressed, and no sentence
            // under a title that already says what the row does.
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
                  <MenuItem
                    title="Keep its sessions"
                    icon={<ProjectsIcon className="size-3.5" />}
                    onSelect={() => drop('detach')}
                  />
                  <MenuItem
                    title="Delete its sessions too"
                    tone="danger"
                    icon={<TrashIcon className="size-3.5" />}
                    onSelect={() => drop('with-sessions')}
                  />
                  {failure && <MenuNote>{failure}</MenuNote>}
                </>
              );
            }
            // COLOUR IS A PALETTE, NOT A COLUMN. Eight named rows stood as tall as the
            // verbs above them and read as eight more of them; a tile says what it does
            // by BEING the colour, so the choice is two rows of four and the group's own
            // is the one wearing the accent frame (the pen strip in `AnnotationLayer`).
            // Nothing is titled over it, either: a strip of colours under the row that
            // asked for them needs no band spelling this group's name a third time, and
            // a tile both picks and returns, so the step keeps its own way out.
            if (step.kind === 'colour')
              return (
                <>
                  <div
                    role="group"
                    aria-label={`Colour for ${band.name}`}
                    className="grid grid-cols-4 justify-items-center gap-1 p-2"
                  >
                    {GROUP_COLORS.map((color) => (
                      <button
                        key={color}
                        type="button"
                        aria-label={`${color[0].toUpperCase()}${color.slice(1)}`}
                        aria-pressed={color === groupColor(band.color)}
                        className="flex min-h-11 min-w-11 shrink-0 items-center justify-center mouse:min-h-9 mouse:min-w-9"
                        onClick={() =>
                          void attempt(
                            () => getClient(conn).updateSessionGroup(band.id, { color }),
                            here,
                          )
                        }
                      >
                        <span
                          className={`size-7 rounded-none border-2 ${groupSwatch(color)} ${
                            color === groupColor(band.color)
                              ? 'border-accent'
                              : 'border-edge-strong'
                          }`}
                        />
                      </button>
                    ))}
                  </div>
                  {failure && <MenuNote>{failure}</MenuNote>}
                </>
              );
            // A group starts its own sessions from the same menu as its other actions.
            return (
              <>
                {!bandArchived && (
                  <MenuItem
                    title="New session"
                     icon={<NewSessionIcon className="size-3.5" />}
                    disabled={creating?.at === creationKey(base, root, band.id)}
                    onSelect={() => {
                      setMenu(null);
                      void onNewSession(conn, root, band.id);
                    }}
                  />
                )}
                <MenuItem
                  title="Rename group"
                  icon={<PencilIcon className="size-3.5" />}
                  onSelect={() => {
                    setTyped(band.name);
                    goTo({ kind: 'rename', id: band.id });
                  }}
                />
                <MenuItem
                  title="Choose colour"
                  icon={<PaletteIcon className="size-3.5" />}
                  onSelect={() => goTo({ kind: 'colour', id: band.id })}
                />
                <MenuItem
                  title={bandArchived ? 'Unarchive group' : 'Archive group'}
                  icon={<ArchiveIcon className="size-3.5" />}
                  onSelect={() =>
                    void attempt(async () => {
                      const changed = await getClient(conn).updateSessionGroup(band.id, {
                        archived: !bandArchived,
                      });
                      setGroups((current) =>
                        current.map((group) => (group.id === changed.id ? changed : group)),
                      );
                      setGroupArchive({
                        id: band.id,
                        archived: !bandArchived,
                        refresh: groupsRead + 1,
                      });
                    }, 'close')
                  }
                />
                <MenuItem
                  title="Delete group"
                  tone="danger"
                  icon={<TrashIcon className="size-3.5" />}
                  onSelect={() => goTo({ kind: 'delete', id: band.id })}
                />
                {failure && <MenuNote>{failure}</MenuNote>}
              </>
            );
          })()}
        </Menu>
      )}
    </>
  );
});
