import { useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState } from 'react';
import { createPortal } from 'react-dom';
import { Banner, LoadMore, overlayLayer } from '../components/ui';
import {
  MachineGap,
  MachineProjectsButton,
  MachineSwitcher,
  MachineTab,
  PullToSearchHint,
} from '../components/SessionNavigator';
import {
  NavigatorSkeleton,
  draftSearchText,
  type SessionListActions,
  type SessionRowAction,
  type SessionRowCommands,
} from '../components/SessionList';
import { SearchMessages } from '../components/SearchMessages';
import { SessionSearchDialog } from '../components/SessionSearchDialog';
import { SearchSessionRows, SessionSearchScopes, useSessionSearchScope, type SearchScope } from './sessions/SessionSearchScopes';
import {
  ProjectGroup,
  creationKey,
  type ProjectCreation,
  type ProjectGroupReading,
  type SessionRowsContext,
} from './sessions/SessionProjectGroups';
import { PANEL_SIZES } from '../components/Menu';
import { GatewayClient, type ProjectWindows, type SessionMatch } from '../lib/gateway';
import { SessionSubscriptionHub } from '../lib/subscriptions';
import type { GatewayConn, Session, SseEvent } from '../lib/types';
import { VIEW_CLOSE_EVENT, VIEW_OPEN_EVENT, viewKind } from '../lib/view';
import { onWake } from '../lib/wake';
import { answeredTurnCount, unreadAfterVisit, unreadTurnCount } from '../lib/unread';
import { reassertBadge, syncBadge } from '../lib/badge';
import { notifyDesktopFleet } from '../lib/desktop-notify';
import { menuPosition, type MenuPosition } from '../lib/anchored-menu';
import {
  applyListScroll,
  forgetListScroll,
  parkedListScroll,
  rowOffset,
  topVisibleRow,
  useListScrollPark,
  type ListAnchor,
} from '../lib/list-scroll';
import { usePullToSearch, type PullPhase } from '../lib/pull-to-search';
import { ManageProjectsSheet, type ManagedProject } from '../components/ManageProjectsSheet';
import { useDeskRail, useFitRows, useMouseDensity } from '../lib/fit-rows';
import { clearMachineOutage, machineOutage, rememberMachineOutage } from '../lib/fleet-outage';
import {
  draftMessageHasUnsent,
  draftMessageKey,
  useDraftMessages,
} from '../lib/draft-messages';
import { shareSummary, type SharedPayload } from '../lib/share-intake';
import { favoriteRank, nextFavoriteRank } from '../lib/favorites';
import {
  fleetError,
  fleetRead,
  machineCounts,
  machineKey,
  machineLabel,
  machineRead,
  projectGroups,
  readSinceCounted,
  searchGroups,
  reconcileMachines,
  resolveScope,
  sameOverview,
  servedUnread,
  scopedMachines,
  searchFanout,
  searchTally,
  sessionIsLive,
  sessionOrder,
  sessionRowKey,
  machineProject,
  type FleetMachine,
  type ProjectGroupView,
} from '../lib/fleet';

const SESSION_LIST_EVENTS = new Set([
  'turn.started',
  'turn.completed',
  'turn.failed',
  'turn.cancelled',
  'session.title_updated',
]);

function isSessionListEvent(event: SseEvent): boolean {
  return (
    SESSION_LIST_EVENTS.has(event.type) ||
    (viewKind(event) === 'input' &&
      (event.type === VIEW_OPEN_EVENT || event.type === VIEW_CLOSE_EVENT))
  );
}

// The frames the FLEET stream sends (`GET /v1/events?scope=fleet`): each one is the
// whole truth about one row — the status the next window read would have carried —
// so a list that already holds that row repaints it and reads nothing at all.
// `session.title_updated` arrives on both streams and is the same fact on either.
const FLEET_ROW_EVENTS = new Set(['session.status', 'session.title_updated']);

// A background poll issued right before the OS suspended the webview can never
// settle: it neither resolves nor rejects after the resume. A plain in-flight
// boolean would then stay latched forever and every later refresh would be
// skipped — the list froze until the app was restarted. Anything older than
// this is treated as lost.
const STALE_POLL_MS = 20_000;

// What the reachability poll costs while the fleet stream carries the news instead.
// It stays a net — a stream can stop without saying so — just a slack one, and a
// stream that drops puts the five-second cadence back on the very next tick.
const STREAMED_POLL_MS = 30_000;

// Searching is a FLEET round trip — one ranked FTS query per paired machine, and on a
// large store the gateway spends ~130ms in SQLite before it answers. Firing that per
// keystroke would queue a search behind every letter of a word and leave the last one
// racing its own predecessors. This pause is what typing RESTING means: it gates the
// needle itself (`searchNeedle`), so it holds back the network AND every re-filter,
// re-rank and re-count the screen does — a keystroke costs the field alone.
const SEARCH_DEBOUNCE_MS = 200;

// Search has an interactive timeout shorter than the general transport budget, so one
// sleeping machine is reported rather than holding the whole fleet.
const SEARCH_REACH_MS = 8_000;

// ONE machine's answer to the live question: the rows its search answered with, in the
// gateway's own order, where each of them matched, and whether the machine ANSWERED AT
// ALL. `reached: false` is not an empty result — it is the absence of one, and the
// screen prints it as such.
type SearchAnswer = {
  matches: SessionMatch[];
  rows: Session[];
  reached: boolean;
  total: number;
  nextCursor: string | null;
  paging?: boolean;
  pageError?: boolean;
};

// A machine that did not answer is not an empty result.
const UNREACHED: SearchAnswer = {
  matches: [], rows: [], reached: false, total: 0, nextCursor: null,
};

async function readSearchPage(
  api: GatewayClient,
  needle: string,
  signal: AbortSignal,
  filters: Parameters<GatewayClient['searchSessions']>[2],
) {
  const reach = new AbortController();
  const giveUp = () => reach.abort();
  signal.addEventListener('abort', giveUp, { once: true });
  if (signal.aborted) giveUp();
  const expiry = window.setTimeout(giveUp, SEARCH_REACH_MS);
  try {
    return await api.searchSessions(needle, reach.signal, filters).catch(() => null);
  } finally {
    window.clearTimeout(expiry);
    // Keep cancellation wired until this query and scope are replaced.
  }
}

// The fleet's answer to ONE needle, `''` being the recents. `asked` is who the question
// went to, so `asked.length - byMachine.size` is exactly how much of the search is still
// outstanding — the progress the screen reports while it waits.
type SearchAnswers = {
  needle: string | null;
  scopeKey: string;
  asked: string[];
  byMachine: Map<string, SearchAnswer>;
};

const NO_MACHINES: string[] = [];

// Nothing asked yet, so no needle is answered: not even the recents' blank one.
const NO_SEARCH: SearchAnswers = {
  needle: null,
  scopeKey: '',
  asked: NO_MACHINES,
  byMachine: new Map(),
};

// A RETRY IS A GESTURE, so it answers on a gesture's clock. The transport gives every
// request 30s (`REQUEST_TIMEOUT_MS`) and a list read can page, so a tile pressed on a
// machine that is blackholed rather than refused — a closed laptop does not refuse a
// socket — wore `Reconnecting…` for half a minute or more. Five seconds of silence IS
// the answer: the probe is cancelled and the tile says so.
const RETRY_TIMEOUT_MS = 5_000;

// The failure is a WORD, not a state: long enough to read, then gone. A verdict left
// standing turns into the strip's own furniture, and the next reader cannot tell it from
// something this machine is still saying.
const RETRY_NOTE_MS = 3_000;

// A SILENT PROBE HAS NOBODY WAITING ON IT, but it must not hold its machine's only
// reconnect slot open forever: a blackholed socket ends when somebody ends it, and the
// next poll is ten seconds away.
const RECONNECT_TIMEOUT_MS = 15_000;

// A MACHINE PAINTED FROM CACHE HAS NOT BEEN HEARD FROM. Its rows are paint, not proof,
// so its first read of the run is bounded: a closed laptop or a peer that dropped off
// the tailnet blackholes the socket rather than refusing it, and under the request
// deadline alone the cached rows stood behind a solid mark for half a minute while the
// machine was off. Nine seconds of silence is the verdict — the same patience the
// Machines screen gives a health probe — and it is recorded as this device's outage.
const COLD_PROBE_TIMEOUT_MS = 9_000;

// Each project PAGES its own history, a gateway-cut window at a time — so the DOM is
// bounded without a global window over the fleet.

// Same frames as the session transcript's spinner and the TUI's
// `paint-content-loading!` — one vocabulary for "working" across the product.
// Two placeholder projects with ragged title widths: an even grid reads as a
// rendered table, a ragged one reads as text that has not arrived yet.
const fleetClients = new Map<string, GatewayClient>();

function clientFor(conn: GatewayConn): GatewayClient {
  const key = `${conn.url}\u0000${conn.token ?? ''}`;
  const existing = fleetClients.get(key);
  if (existing) return existing;
  const client = new GatewayClient({ url: conn.url, token: conn.token });
  fleetClients.set(key, client);
  return client;
}

/** Read a session's transcript ahead of the click that is about to open it. */
function warmTranscript(conn: GatewayConn, session: Session): void {
  clientFor(conn).warmTranscript(session);
}

// Persist known outages across remounts and start those machines drained until they
// answer again.

// Confirm an outage after two misses for a machine that previously answered; a cold
// machine with no rows can fail immediately.
const fleetMisses = new Map<string, number>();

// Two consecutive failed reads confirm an outage.
const OUTAGE_CONFIRMING_MISSES = 2;

// Reconcile paired machines with cached rows and remembered outage state.
function hydrateMachines(conns: GatewayConn[], previous: FleetMachine[]): FleetMachine[] {
  return reconcileMachines(conns, previous).map((machine) => {
    if (machine.error !== null) return machine;
    const outage = machineOutage(machineKey(machine.conn));
    // Cached rows are paint data, not proof of current reachability.
    if (machine.sessions !== null)
      return outage ? { ...machine, error: outage, isRemembered: true } : machine;
    const api = clientFor(machine.conn);
    // Seed machine-wide totals alongside cached rows.
    const overview = machine.overview ?? api.cachedProjectsOverview();
    const cached = api.cachedSessions();
    if (!cached && !outage) return overview ? { ...machine, overview } : machine;
    return {
      ...machine,
      sessions: cached ?? null,
      error: outage,
      isRemembered: outage !== null,
      overview,
    };
  });
}

/**
 * Derive project page size from measured screen geometry. Touch retains useful row
 * density; pointer layouts keep a smaller emergency floor, and each layout reserves a
 * peek at the following project.
 */
const LIST_PEEK = 40;
const LIST_FOOT = 16;
const LIST_GEOMETRY = {
  // Outside the desk rail, numeric pages occupy their own line in the project band.
  touch: { row: 49, chrome: 211 + 44 + LIST_PEEK, min: 15 },
  mouse: { row: 33, chrome: 149 + 40 + LIST_FOOT + LIST_PEEK, min: 3 },
  // The wider desk rail shows row metadata, inline pages and its own 28px footer.
  desk: { row: 49, chrome: 149 + 28 + LIST_PEEK, min: 3 },
} as const;

/** Page size sent to the gateway for the current measured layout. */
function useSessionsPerPage(): number {
  const isMouse = useMouseDensity();
  const isDesk = useDeskRail();
  const geometry = isDesk
    ? LIST_GEOMETRY.desk
    : isMouse
      ? LIST_GEOMETRY.mouse
      : LIST_GEOMETRY.touch;
  return useFitRows(geometry);
}

/**
 * How wide the folder browser is placed from. The sheet is RIGHT-aligned to the
 * control it hangs from, so the anchor math needs the width before it has ever been
 * measured — which is exactly why the number and the class that paints it live
 * together in `PANEL_SIZES` rather than being restated here.
 */
const BROWSE_WIDTH = PANEL_SIZES.browse.width;

interface Props {
  /** Every paired machine, in pairing order. This screen renders the FLEET. */
  conns: GatewayConn[];
  /** The machine that leads and initially owns the sessions scope. */
  primary?: GatewayConn | null;
  /** The search the dialog asks. It is empty while the dialog is closed. */
  query: string;
  onQuery: (next: string) => void;
  subscriptions: SessionSubscriptionHub | null;
  /** No machine is answering at all — the shell decides what to show instead. */
  onUnreachable?: (message: string | null) => void;
  onOpen: (conn: GatewayConn, sid: string, fresh?: boolean) => void | Promise<void>;
  /**
   * The session standing open in the pane beside this list, so the row it belongs to
   * can say so. `fresh` identifies a session this device just created, including a
   * fork made in the transcript rather than through the list. `null` while nothing
   * is open — and on a phone, where the transcript replaces the list.
   */
  openSession?: { conn: GatewayConn; sid: string; fresh?: boolean } | null;
  /**
   * Whether this screen is the one on the glass. It stays MOUNTED behind an open
   * transcript — its rows, scope, scroll position and expanded projects are the
   * reader's own frame — and everything below reads this to keep fleet-wide work
   * off a screen nobody can see.
   */
  isVisible: boolean;
  /**
   * Open the search dialog — the same door the app bar's glass is. It is `null`
   * while the dialog is ALREADY open, which stands the list's pull-down gesture
   * (`lib/pull-to-search`) down: a hint promising a dialog the reader is already
   * in is the screen lying to them.
   */
  onSearch: (() => void) | null;
  /** Whether the search dialog stands over the app. The list under it never filters. */
  isSearchOpen: boolean;
  /** Leave the search dialog. The shell clears the query with it. */
  onCloseSearch: () => void;
  /**
   * A share the OS handed over that no composer has taken yet. THIS LIST IS THE
   * CHOOSER — only the human knows whether a voice memo belongs to a session
   * that is already running or to a new one — so while a payload is parked the
   * list says what is waiting and every row is a destination.
   */
  share?: SharedPayload | null;
  /** Throw the parked share away, staged files included. */
  onDiscardShare?: () => void;
}

export function SessionsScreen({
  conns,
  primary = null,
  query,
  onQuery,
  subscriptions,
  onUnreachable,
  onOpen,
  openSession = null,
  isVisible,
  onSearch,
  isSearchOpen,
  onCloseSearch,
  share = null,
  onDiscardShare,
}: Props) {
  const primaryKey = primary ? machineKey(primary) : null;
  // A machine OWNS its projects: every row belongs to exactly one gateway, and a
  // project only exists inside the machine it lives on. The fleet is therefore
  // one entry per paired machine, seeded from that machine's last known list so
  // returning to this tab repaints the previous frame instantly; the effects
  // below revalidate each machine independently and reconcile on top.
  const [machines, setMachines] = useState<FleetMachine[]>(() => hydrateMachines(conns, []));
  // EVERY ACCEPTED SNAPSHOT OF A MACHINE, whether it moved anything or not. The gateway
  // owns the session groups and sends no fleet frame when one is renamed, so this list's
  // own read is the only thing that revalidates the wall of a project already on screen.
  const [machineReads, setMachineReads] = useState<ReadonlyMap<string, number>>(() => new Map());
  // ONLY THE SNAPSHOTS THAT MOVED SOMETHING, which is what a project's deeper page
  // windows are revalidated from. An idle poll that answers with the rows already on
  // screen is not news (see `settle`), and it must not charge the machine a full prefetch
  // of every project on screen to learn that.
  const [machineMoves, setMachineMoves] = useState<ReadonlyMap<string, number>>(() => new Map());
  // A visit is local truth before the transcript's gateway read mark reaches this list.
  const [readFloors, setReadFloors] = useState<ReadonlyMap<string, number>>(() => new Map());
  const readFloorsRef = useRef(readFloors);
  // Exactly one paired machine is always active. The saved primary owns the first
  // scope; if it changes while this mounted screen is behind Settings, it becomes
  // the scope on return. Pressing the selected tab cannot turn it off.
  const [scopePick, setScopePick] = useState<string | null>(
    () => primaryKey ?? (conns[0] ? machineKey(conns[0]) : null),
  );
  useEffect(() => {
    if (primaryKey) setScopePick(primaryKey);
  }, [primaryKey]);
  const scope = resolveScope(machines, scopePick);

  // How far this run has got with the machines on screen. Everything below that has a
  // loading state reads this one value, and a machine's own tile and section read
  // `machineRead` of that machine; see `MachineRead` for the rule it states.
  const fleetState = fleetRead(machines, scope);
  // Keep raw field input separate from the settled search needle; filtering, ranking
  // and network work update only once typing pauses.
  const [searchNeedle, setSearchNeedle] = useState(() => query.trim());
  useEffect(() => {
    const next = query.trim();
    if (next === searchNeedle) return;
    // CLEARING IS NOT A SEARCH. An empty field asks no gateway anything, so the list
    // comes back on the same frame as the empty box instead of a pause later.
    if (!next) {
      setSearchNeedle('');
      return;
    }
    const timer = window.setTimeout(() => setSearchNeedle(next), SEARCH_DEBOUNCE_MS);
    return () => window.clearTimeout(timer);
  }, [query, searchNeedle]);
  // Every machine's answer to ONE question, filed under the needle it answered — a
  // fleet search is several round trips that land at different times, and the
  // screen has to be able to say which of them are still out. A machine answers
  // with its rows and where they matched in the same read.
  const [searchAnswers, setSearchAnswers] = useState<SearchAnswers>(NO_SEARCH);
  const searchControllerRef = useRef<AbortController | null>(null);
  const searchPagesRef = useRef(new Set<string>());
  // The create in flight and the project header that started it. Only that
  // header replaces its plus with the busy word.
  const [creating, setCreating] = useState<{
    at: string | null;
    label: string;
  } | null>(null);
  const [createError, setCreateError] = useState<string | null>(null);
  const [manageProjects, setManageProjects] = useState<{
    machine: FleetMachine;
    at: MenuPosition;
  } | null>(null);
  const pollStartedAt = useRef<number | null>(null);
  // Is the gateway pushing this list its fleet status, and when did the window last
  // cost a read? Refs, not state: the poll reads them on its own tick, and a cadence
  // must not re-run the effect that owns the timer.
  const fleetStreamingRef = useRef(false);
  const lastWindowReadAt = useRef(0);
  // Only in-flight reads retain newer frames; settled snapshots need no overlay cache.
  // null parks a row until the post-terminal read has warmed its transcript.
  const fleetReads = useRef(
    new Set<{
      key: string;
      updates: Map<string, Partial<Session> | null>;
      superseded: boolean;
    }>(),
  );
  const fleetRefreshQueued = useRef(false);
  // The row action belongs to one session on one machine. Renaming edits its title
  // inline; deleting asks through `ConfirmRow` exactly where that session row stood.
  // Forking from the row copies the whole conversation; a turn cuts it in the transcript.
  const [rowAction, setRowAction] = useState<SessionRowAction | null>(null);
  const [actionBusy, setActionBusy] = useState(false);
  const [actionError, setActionError] = useState<string | null>(null);
  const listRef = useRef<HTMLDivElement>(null);
  const hintRef = useRef<HTMLDivElement>(null);
  // How far the finger has pulled the top of the list down, in the only three
  // steps the screen paints. It changes at most twice per gesture, never per frame.
  const [pullPhase, setPullPhase] = useState<PullPhase>('none');
  const refreshAnchorRef = useRef<ListAnchor | null>(null);
  // The reading position is put back at most once per mount, and never after the
  // reader has taken the scroller over.
  const restoredRef = useRef(false);
  const connsRef = useRef(conns);
  const machinesRef = useRef(machines);
  // MACHINES THAT WENT SILENT ON A SEARCH. A ref, not state: nothing on screen reads it
  // (the answers already say who could not be reached) and a re-render for it would
  // restart the very effect that writes it.
  const searchSilentRef = useRef(new Set<string>());
  // Refs mirror the latest props for callbacks that must not re-subscribe on every
  // connection object identity change. Written in an effect so render stays pure.
  useEffect(() => {
    connsRef.current = conns;
    machinesRef.current = machines;
  });
  // Transport identity of the WHOLE fleet: pairing, unpairing or re-tokening a
  // machine reloads it; renaming one never does.
  const fleetKey = conns.map((conn) => `${conn.url}\u0000${conn.token ?? ''}`).join('|');
  // Unsent words this device is holding, keyed by (gateway, session). An EMPTY
  // session that has some is DIRTY: it stays in the list — with a way back into
  // what you wrote, and a way to throw it away — instead of being hidden with
  // the words locked inside it.
  const draftMessages = useDraftMessages(isVisible);

  // The app icon's badge is the SAME tally as the dots on these rows: one per
  // answer this device has not read. It is written from here because this is
  // the only place that sees every machine at once — and said again on wake,
  // because `VisNotify` moved the number while the app was away and told
  // nobody. `syncBadge` also drops the delivered alerts of sessions that have
  // since been read, which is what keeps the extension's count honest.
  useEffect(() => {
    void syncBadge(machines);
  }, [machines]);
  useEffect(() => onWake(() => void reassertBadge()), []);

  // The desktop window receives no push at all, so it raises its own alerts from the fleet it is
  // already polling: a new answer or a new question while the app is open, and nothing on the
  // first pass, because opening the app is not news.
  useEffect(() => {
    void notifyDesktopFleet(machines);
  }, [machines]);

  // Anchor the top visible row around every asynchronous fleet mutation so staggered
  // machine responses cannot move content under the reader.
  const patchMachine = useCallback(
    (key: string, update: (machine: FleetMachine) => FleetMachine) => {
      refreshAnchorRef.current = topVisibleRow(listRef.current);
      setMachines((current) => {
        const index = current.findIndex((machine) => machineKey(machine.conn) === key);
        // Unpaired while its request was in flight: the answer is not fleet news.
        if (index < 0) return current;
        const next = update(current[index]);
        if (next === current[index]) return current;
        const copy = current.slice();
        copy[index] = next;
        return copy;
      });
    },
    [],
  );

  // Carry a visit through the list's slower reads, including an in-flight window.
  const noteOpenedRead = useCallback(
    (conn: GatewayConn, session: Session, seen = answeredTurnCount(session)) => {
      const rowKey = sessionRowKey(conn, session.id);
      if ((readFloorsRef.current.get(rowKey) ?? -1) >= seen) return;
      const next = new Map(readFloorsRef.current).set(rowKey, seen);
      readFloorsRef.current = next;
      setReadFloors(next);
      patchMachine(machineKey(conn), (machine) => {
        if (!machine.sessions) return machine;
        const previous = machine.sessions;
        const rows = machine.sessions.map((row) => {
          if (row.id !== session.id) return row;
          const unread = unreadAfterVisit(row, seen);
          return unread < unreadTurnCount(row)
            ? { ...row, is_unread: unread > 0, unread_answers: unread }
            : row;
        });
        return rows.some((row, index) => row !== previous[index])
          ? { ...machine, sessions: rows }
          : machine;
      });
    },
    [patchMachine],
  );
  // Row taps know their page's exact row; deep links use the best cached copy.
  useEffect(() => {
    if (!openSession) return;
    const { conn, sid } = openSession;
    const row =
      machinesRef.current.find((machine) => machineKey(machine.conn) === machineKey(conn))
        ?.sessions?.find((session) => session.id === sid) ?? clientFor(conn).cachedSession(sid);
    if (row) noteOpenedRead(conn, row);
  }, [openSession?.conn.url, openSession?.sid, noteOpenedRead]);

  // Closing the pane can follow another answer. Use the transcript's latest cached
  // read mark before the list is painted again; do not hide an unseen newer answer.
  const previouslyOpen = useRef(openSession);
  useLayoutEffect(() => {
    const prior = previouslyOpen.current;
    previouslyOpen.current = openSession;
    if (!prior) return;
    if (
      openSession &&
      sessionRowKey(prior.conn, prior.sid) === sessionRowKey(openSession.conn, openSession.sid)
    ) return;
    const cached = clientFor(prior.conn).cachedSession(prior.sid);
    if (cached)
      noteOpenedRead(prior.conn, cached, answeredTurnCount(cached) - unreadTurnCount(cached));
  }, [openSession, noteOpenedRead]);

  // ONE machine's list. Machines load independently on purpose: a gateway that is
  // asleep must not keep the machines next to it off the screen, and its failure
  // drops that machine out of the fleet view instead of taking the whole list down.
  //
  // It ANSWERS its failure as well as storing it: a retry the reader asked for has to
  // say what came back, in the tile that was pressed, and reading that off the state
  // this call is about to write would be reading it a paint too early.
  const loadMachine = useCallback(
    async (
      conn: GatewayConn,
      signal?: AbortSignal,
      deadlineMs?: number,
    ): Promise<string | null> => {
      const key = machineKey(conn);
      const api = clientFor(conn);
      const updates = new Map<string, Partial<Session> | null>();
      const flight = { key, updates, superseded: false };
      fleetReads.current.add(flight);
      // A BOUNDED READ FAILS ON ITS OWN DEADLINE. The outer signal is a cancellation —
      // the screen went away, nobody is owed an answer — and stays one. The deadline is
      // this read's own verdict: the machine did not speak, and the tile must say so.
      const probe = deadlineMs === undefined ? null : new AbortController();
      const cancel = () => probe?.abort();
      signal?.addEventListener('abort', cancel, { once: true });
      const giveUp =
        probe === null ? undefined : window.setTimeout(() => probe.abort(), deadlineMs);
      const silence =
        deadlineMs === undefined ? null : `silent for ${Math.round(deadlineMs / 1000)}s`;
      // ANY answer from this machine ends its darkness — both kinds. A gateway that was
      // merely busy when a search ran out of deadline must not stay skipped: the 10s poll
      // is what proves it alive again, and only a machine that keeps failing keeps being
      // passed over (see `searchFanout`). The same answer ends its OUTAGE: a machine is
      // back in `All` and back on the foreground load because it spoke, never because it
      // was asked.
      const alive = () => {
        searchSilentRef.current.delete(key);
        clearMachineOutage(key);
        fleetMisses.delete(key);
      };
      try {
        // A poll that answers with the rows already on screen is NOT NEWS. Patching
        // anyway handed the list a new fleet array every ten seconds, and every memo
        // built from it — the scope filter, the sort, the project grouping, the pager
        // — re-ran under a reader who was only reading.
        const settle = (rows: Session[]) => {
          const held = machinesRef.current.find((machine) => machineKey(machine.conn) === key);
          const current = rows.flatMap((row) => {
            const update = updates.get(row.id);
            if (update === null) {
              const previous = held?.sessions?.find((session) => session.id === row.id);
              return previous ? [previous] : [];
            }
            const received = update ? { ...row, ...update } : row;
            const seen = readFloorsRef.current.get(sessionRowKey(conn, row.id));
            const unread = unreadAfterVisit(received, seen);
            const nextRow = unread < unreadTurnCount(received)
              ? { ...received, is_unread: unread > 0, unread_answers: unread }
              : received;
            return [nextRow];
          });
          const merged = reconcileSessions(held?.sessions ?? null, current);
          // The stable project totals arrive BESIDE the head window. Adopt both in one
          // patch so no intermediate frame tallies whichever session pages landed first.
          const reportedOverview = api.projectsOverview();
          const overview =
            reportedOverview && held?.overview && sameOverview(held.overview, reportedOverview)
              ? held.overview
              : reportedOverview;
          // Rows are muted above for visits made here, but the overview counted what the
          // gateway SERVED: that verdict travels beside the rows it no longer matches.
          const countedUnread = servedUnread(rows, held?.countedUnread);
          setMachineReads((current) => new Map(current).set(key, (current.get(key) ?? 0) + 1));
          if (
            held &&
            held.error === null &&
            held.answered &&
            merged === held.sessions &&
            overview === held.overview &&
            countedUnread === held.countedUnread
          )
            return;
          setMachineMoves((current) => new Map(current).set(key, (current.get(key) ?? 0) + 1));
          patchMachine(key, (machine) => ({
            ...machine,
            sessions: merged,
            overview,
            countedUnread,
            error: null,
            answered: true,
            isRemembered: false,
          }));
        };
        const next = await api.listSessions(probe?.signal ?? signal);
        if (signal?.aborted || flight.superseded) return null;
        // Set insertion order is request order. A newer accepted snapshot supersedes
        // older reads of this machine, not a later read that is still in flight.
        for (const pending of fleetReads.current) {
          if (pending === flight) break;
          if (pending.key === key) pending.superseded = true;
        }
        alive();
        settle(next);
        return null;
      } catch (cause) {
        if (signal?.aborted || flight.superseded) return null;
        const failure =
          probe?.signal.aborted && silence !== null ? silence : (cause as Error).message;
        const held = machinesRef.current.find((machine) => machineKey(machine.conn) === key);
        // ONE MISSED READ IS NOT AN OUTAGE (see `fleetMisses`): a machine that was
        // answering a moment ago gets the next read before this device calls it dark.
        const misses = (fleetMisses.get(key) ?? 0) + 1;
        fleetMisses.set(key, misses);
        if (misses < OUTAGE_CONFIRMING_MISSES && held?.answered && held.error === null)
          return failure;
        rememberMachineOutage(key, failure);
        // A FAILURE THAT SAYS NOTHING NEW IS NOT NEWS EITHER (same rule as `settle`).
        // Re-patching an unchanged verdict handed the list a new fleet array on every
        // poll — and with it a re-anchored scroll position and every memo built from the
        // fleet — for a machine that has not been on screen since it went dark.
        // A REMEMBERED failure is not this same verdict: this read is what CONFIRMS the
        // darkness in THIS run, and the shell's offline gate waits for exactly that.
        if (held?.error !== failure || held.isRemembered)
          patchMachine(key, (machine) => ({
            ...machine,
            error: failure,
            answered: false,
            isRemembered: false,
          }));
        return failure;
      } finally {
        fleetReads.current.delete(flight);
        if (giveUp !== undefined) window.clearTimeout(giveUp);
        signal?.removeEventListener('abort', cancel);
      }
    },
    [patchMachine],
  );

  // Optimistically update the star, revert on failure, then reload gateway-owned order.
  const toggleStar = useCallback(
    (session: Session, conn: GatewayConn) => {
      const key = machineKey(conn);
      const api = clientFor(conn);
      const before = favoriteRank(session);
      const starring = before === null;
      const withRank = (rank: number | null) => (machine: FleetMachine) =>
        machine.sessions
          ? {
              ...machine,
              sessions: machine.sessions.map((row) =>
                row.id === session.id ? { ...row, favorite_rank: rank } : row,
              ),
            }
          : machine;
      patchMachine(key, (machine) =>
        withRank(starring ? nextFavoriteRank(machine.sessions ?? []) : null)(machine),
      );
      void api
        .setSessionFavorite(session.id, starring)
        .then(async (row) => {
          patchMachine(key, withRank(favoriteRank(row)));
          await loadMachine(conn);
        })
        .catch(() => patchMachine(key, withRank(before)));
    },
    [loadMachine, patchMachine],
  );

  // A down machine tile retries in place and reports only active progress or failure.
  const [retries, setRetries] = useState<ReadonlyMap<string, 'busy' | 'failed'>>(() => new Map());
  // Each tile's pending expiry, so a second press cancels the first press's word
  // instead of inheriting the moment it vanishes — and an unmount takes them all.
  const noteExpiry = useRef(new Map<string, number>());
  useEffect(
    () => () => {
      for (const timer of noteExpiry.current.values()) window.clearTimeout(timer);
      noteExpiry.current.clear();
    },
    [],
  );
  const retryMachine = useCallback(
    async (conn: GatewayConn) => {
      const key = machineKey(conn);
      const pending = noteExpiry.current.get(key);
      if (pending !== undefined) {
        window.clearTimeout(pending);
        noteExpiry.current.delete(key);
      }
      setRetries((current) => new Map(current).set(key, 'busy'));
      // The deadline CANCELS the probe and answers the press by itself: a blackholed
      // socket only ends when someone aborts it, and a transport that ignores the
      // cancellation must not be able to hold the word inside the tile.
      const deadline = new AbortController();
      let giveUp: number | undefined;
      const expired = new Promise<true>((resolve) => {
        giveUp = window.setTimeout(() => resolve(true), RETRY_TIMEOUT_MS);
      });
      const failed = await Promise.race([
        loadMachine(conn, deadline.signal).then((failure) => failure !== null),
        expired,
      ]);
      if (giveUp !== undefined) window.clearTimeout(giveUp);
      // A probe that lost the race is over: its late answer must not repaint a tile
      // the reader has already been told about.
      deadline.abort();
      setRetries((current) => {
        const next = new Map(current);
        if (failed) next.set(key, 'failed');
        else next.delete(key);
        return next;
      });
      if (!failed) return;
      noteExpiry.current.set(
        key,
        window.setTimeout(() => {
          noteExpiry.current.delete(key);
          setRetries((current) => {
            if (current.get(key) !== 'failed') return current;
            const next = new Map(current);
            next.delete(key);
            return next;
          });
        }, RETRY_NOTE_MS),
      );
    },
    [loadMachine],
  );

  // Probe down machines independently and at most once each; never let an absent
  // gateway delay polling machines that answer.
  const reconnecting = useRef(new Map<string, () => void>());
  useEffect(
    () => () => {
      for (const cancel of reconnecting.current.values()) cancel();
      reconnecting.current.clear();
    },
    [],
  );
  const reconnectMachine = useCallback(
    (conn: GatewayConn) => {
      const key = machineKey(conn);
      if (reconnecting.current.has(key)) return;
      const deadline = new AbortController();
      let giveUp: number | undefined;
      const done = () => {
        if (giveUp !== undefined) window.clearTimeout(giveUp);
        reconnecting.current.delete(key);
      };
      reconnecting.current.set(key, () => {
        deadline.abort();
        done();
      });
      giveUp = window.setTimeout(() => deadline.abort(), RECONNECT_TIMEOUT_MS);
      void loadMachine(conn, deadline.signal).finally(done);
    },
    [loadMachine],
  );

  const load = useCallback(
    async (signal?: AbortSignal, background = false) => {
      if (background) {
        const started = pollStartedAt.current;
        if (started !== null && Date.now() - started < STALE_POLL_MS) return;
        pollStartedAt.current = Date.now();
      }
      // A machine already known dark is not part of this load at all: it is reconnected
      // BESIDE it, so it can neither hold the fleet's refresh open nor repaint a list it
      // is not in.
      const paired = connsRef.current;
      const dark = (conn: GatewayConn) => machineOutage(machineKey(conn)) !== null;
      for (const conn of paired.filter(dark)) reconnectMachine(conn);
      // A machine that has not answered in THIS run is read under `COLD_PROBE_TIMEOUT_MS`:
      // its cached rows are on screen behind an outline, and the outline must resolve.
      const unconfirmed = (conn: GatewayConn) =>
        machinesRef.current.find((machine) => machineKey(machine.conn) === machineKey(conn))
          ?.answered !== true;
      try {
        do {
          if (background) fleetRefreshQueued.current = false;
          await Promise.all(
            paired
              .filter((conn) => !dark(conn))
              .map((conn) =>
                loadMachine(conn, signal, unconfirmed(conn) ? COLD_PROBE_TIMEOUT_MS : undefined),
              ),
          );
          // A terminal or reconnect during a slow poll still needs one fresh read.
          // Ordinary timer ticks remain droppable, and every response can paint.
        } while (background && fleetRefreshQueued.current && !signal?.aborted);
      } finally {
        if (background) pollStartedAt.current = null;
      }
    },
    [loadMachine, reconnectMachine],
  );

  // Draft presence changes the gateway-owned order, so reload when it changes.
  const dirtyOverlay = useMemo(
    () =>
      Object.entries(draftMessages)
        .filter(([, message]) => draftMessageHasUnsent(message))
        .map(([key]) => key)
        .sort()
        .join('|'),
    [draftMessages],
  );

  // Rehydrate connection metadata without refetching rows when labels, IDs or recovered
  // addresses change.
  const fleetFacts = conns
    .map(
      (conn) =>
        `${conn.url}\u0000${conn.label ?? ''}\u0000${conn.id ?? ''}\u0000${(conn.alts ?? []).join(' ')}`,
    )
    .join('|');
  useEffect(() => {
    setMachines((current) => hydrateMachines(connsRef.current, current));
  }, [fleetFacts]);

  // Pairing changes rebuild the fleet; machines that stayed keep their rows.
  useEffect(() => {
    setMachines((current) => hydrateMachines(connsRef.current, current));
  }, [fleetKey]);

  // Prime the list behind an open transcript once, using cached rows when available;
  // visible polling remains separate.
  const hasRows = machines.some((machine) => machine.sessions !== null);
  useEffect(() => {
    if (isVisible || hasRows) return;
    const controller = new AbortController();
    void load(controller.signal);
    return () => controller.abort();
  }, [fleetKey, hasRows, isVisible, load]);

  // Behind an open transcript this screen is mounted but invisible, and a list
  // nobody can see must not do fleet-wide work: this poll refetched every machine
  // every 5s and re-ran the filter and the sort of the whole fleet — under the
  // composer the reader was typing in. Becoming visible re-runs the effect, whose
  // first act is a full load, so the rows are fresh the moment they are back on
  // the glass.
  useEffect(() => {
    if (!isVisible) return;
    const controller = new AbortController();
    const refreshLiveStates = () => {
      // This tick is a REACHABILITY probe, and the fleet stream is a better one:
      // while it delivers, every transition arrives as a frame and this read is only
      // the net under a stream that stopped. So keep the tick, slow the READ.
      const now = Date.now();
      if (fleetStreamingRef.current && now - lastWindowReadAt.current < STREAMED_POLL_MS) return;
      lastWindowReadAt.current = now;
      void load(controller.signal, true);
    };

    void load(controller.signal);
    lastWindowReadAt.current = Date.now();
    // The session-list request is also the reachability check. Drop overlapping polls
    // and do not trust mobile `visibilityState` as the sole visibility signal.
    const timer = window.setInterval(refreshLiveStates, 5_000);
    // Waking is the one moment the rows are guaranteed stale, and a suspended
    // poll may still be latched: drop the latch, then refresh.
    const stopWake = onWake(() => {
      pollStartedAt.current = null;
      refreshLiveStates();
    });
    return () => {
      controller.abort();
      window.clearInterval(timer);
      stopWake();
    };
    // A connection identity change should preserve the existing frame until its data arrives.
  }, [dirtyOverlay, fleetKey, isVisible, load]);

  // Apply a successful gateway deletion to exactly the machine that owned it. The
  // gateway already answered which ids disappeared, so neither a session nor a project
  // removal re-downloads the fleet merely to rediscover that answer.
  const forgetSessions = useCallback(
    (conn: GatewayConn, ids: string[], project?: ManagedProject) => {
      const api = clientFor(conn);
      for (const sid of ids) api.forgetDeletedSession(sid);

      const gone = new Set(ids);
      patchMachine(machineKey(conn), (machine) => {
        const rows = machine.sessions;
        const sessions = rows && gone.size > 0 ? rows.filter((row) => !gone.has(row.id)) : rows;
        let overview = machine.overview;
        if (project && overview) {
          const projects = overview.projects.filter((entry) =>
            project.projectId
              ? entry.project_id !== project.projectId
              : entry.root !== project.root,
          );
          if (projects.length !== overview.projects.length) {
            overview = {
              ...overview,
              projects,
              project_count: projects.length,
              session_count: projects.reduce((total, entry) => total + entry.session_count, 0),
              live_count: projects.reduce((total, entry) => total + entry.live_count, 0),
              awaiting_count: projects.reduce((total, entry) => total + entry.awaiting_count, 0),
              ...(typeof overview.unread_count === 'number' && {
                unread_count: projects.reduce((total, entry) => total + (entry.unread_count ?? 0), 0),
              }),
            };
          }
        }
        if (sessions === rows && overview === machine.overview) return machine;
        return { ...machine, sessions, overview };
      });
    },
    [patchMachine],
  );

  // Titles and badges can update in place. Starting a run changes its rank, so
  // refresh the canonical window as well. A settled frame also needs that read to
  // warm the finished transcript before replacing LIVE with NEW.
  const applyFleetFrame = useCallback(
    (event: SseEvent): boolean => {
      // A copied title frame lives in every watched session's replay ring. Its
      // `session_id` names that ring; `titled_session_id` names the row that changed.
      const sid =
        typeof event.titled_session_id === 'string'
          ? event.titled_session_id
          : typeof event.session_id === 'string'
            ? event.session_id
            : typeof event.sid === 'string'
              ? event.sid
              : '';
      if (!sid) return false;
      let update: Partial<Session>;
      if (event.type === 'session.status') {
        if (typeof event.is_live !== 'boolean') return false;
        if (!event.is_live) {
          for (const { updates } of fleetReads.current) updates.set(sid, null);
          return false;
        }
        update = {
          live: event.is_live,
          is_awaiting_input: event.is_awaiting_input === true,
          awaiting_input_count:
            typeof event.awaiting_input_count === 'number' ? event.awaiting_input_count : 0,
          current_turn_id: typeof event.current_turn_id === 'string' ? event.current_turn_id : null,
        };
      } else if (typeof event.title === 'string' && event.title.length > 0) {
        update = { title: event.title };
      } else {
        return false;
      }
      for (const { updates } of fleetReads.current) {
        const previous = updates.get(sid);
        if (previous !== null || event.type === 'session.status')
          updates.set(sid, { ...previous, ...update });
      }
      const holders = machinesRef.current.filter((machine) =>
        machine.sessions?.some((row) => row.id === sid),
      );
      const orderChanged =
        event.type === 'session.status' &&
        holders.some((machine) =>
          machine.sessions?.some((row) => row.id === sid && row.live !== update.live),
        );
      for (const machine of holders)
        patchMachine(machineKey(machine.conn), (current) =>
          current.sessions
            ? {
                ...current,
                sessions: current.sessions.map((row) =>
                  row.id === sid ? { ...row, ...update } : row,
                ),
              }
            : current,
        );
      return holders.length > 0 && !orderChanged;
    },
    [patchMachine],
  );

  useEffect(() => {
    // Same rule as the poll above: a lifecycle event cannot move a list nobody is
    // looking at, and the load on becoming visible answers with the gateway's
    // canonical order anyway.
    if (!subscriptions || !isVisible) return;
    let refreshTimer: number | null = null;
    const readWindow = () => {
      if (refreshTimer !== null) window.clearTimeout(refreshTimer);
      // Coalesce lifecycle bursts, then ask the gateway for its canonical order.
      refreshTimer = window.setTimeout(() => {
        fleetRefreshQueued.current = true;
        void load(undefined, true);
      }, 120);
    };
    const stopState = subscriptions.subscribeFleetState((streaming) => {
      if (streaming && !fleetStreamingRef.current) {
        readWindow();
      }
      fleetStreamingRef.current = streaming;
    });
    const unsubscribe = subscriptions.subscribeFleet((event) => {
      // Ready is sent after the server registered this subscriber. An HTTP open
      // alone can precede registration, and reconnects carry no replay.
      if (event.type === 'subscription.ready' && event.scope === 'fleet') {
        readWindow();
        return;
      }
      if (event.type === 'session.deleted') {
        const sid = event.session_id ?? event.sid;
        if (!sid) return;
        for (const machine of machinesRef.current) {
          if (clientFor(machine.conn).base === subscriptions.gatewayUrl) {
            forgetSessions(machine.conn, [sid]);
          }
        }
        return;
      }
      if (FLEET_ROW_EVENTS.has(event.type)) {
        if (!applyFleetFrame(event)) readWindow();
        return;
      }
      // The multiplexed SESSION stream reaches only what this device has VISITED, and
      // it is what this list ran on before the fleet stream existed. While that stream
      // delivers it is the authority and these frames are its echo.
      if (fleetStreamingRef.current) return;
      if (!isSessionListEvent(event)) return;
      readWindow();
    });
    return () => {
      unsubscribe();
      stopState();
      fleetStreamingRef.current = false;
      if (refreshTimer !== null) window.clearTimeout(refreshTimer);
    };
  }, [applyFleetFrame, forgetSessions, isVisible, load, subscriptions]);

  useLayoutEffect(() => {
    const anchor = refreshAnchorRef.current;
    const viewport = listRef.current;
    refreshAnchorRef.current = null;
    if (!anchor || !viewport || viewport.scrollTop <= 2) return;
    const offset = rowOffset(viewport, anchor.id);
    if (offset !== null) viewport.scrollTop += offset - anchor.offset;
  }, [machines]);

  // A phone keeps this list mounted behind a session, but the hidden webview
  // may reset its scroller. Restore the last visible mark when it comes back,
  // once the rows are ready; a fresh gesture always takes precedence.
  const wasVisibleRef = useRef(isVisible);
  useLayoutEffect(() => {
    if (isVisible && !wasVisibleRef.current) restoredRef.current = false;
    wasVisibleRef.current = isVisible;
  }, [isVisible]);
  useLayoutEffect(() => {
    if (restoredRef.current || !isVisible) return;
    const mark = parkedListScroll();
    if (!mark) {
      restoredRef.current = true;
      return;
    }
    const viewport = listRef.current;
    if (!viewport) return;
    // A mark that cannot fit after every machine answered names rows that are
    // gone; retrying it on later paints would fight the reader.
    if (applyListScroll(viewport, mark, (id) => rowOffset(viewport, id)) || fleetState !== 'reading') {
      restoredRef.current = true;
      forgetListScroll();
    }
  });

  // The reader scrolling is the reader deciding: stop trying to restore.
  useListScrollPark(
    listRef,
    () => {
      restoredRef.current = true;
    },
    isVisible,
  );

  // AT THE TOP OF THE LIST, A PULL IS A QUESTION ABOUT SEARCH. The glass that opens
  // the search dialog sits in the far top corner of the app bar; the thumb already
  // reading this list has a gesture for it, and every native list answers it.
  usePullToSearch(listRef, hintRef, setPullPhase, onSearch);

  // THE QUESTION ON THE WIRE while the dialog is open: the settled needle, or the
  // RECENTS (`''`) while the field is blank, as the terminal's switcher lists them
  // before a word is typed. Words the pause has not settled yet ask nothing: recents
  // asked then would be a round trip nobody waits for.
  const searchQuestion = !isSearchOpen ? null : searchNeedle || (query.trim() ? null : '');
  const searchScope = useSessionSearchScope(conns, isSearchOpen, scope, clientFor);
  const searchFilter = searchScope.wire;

  // Ask once per settled question, abort superseded requests, ignore late answers by
  // key, and paint each machine as soon as its answer arrives.
  useEffect(() => {
    const needle = searchQuestion;
    if (needle === null) return;
    // WHICH MACHINES ARE EVEN ASKED. A gateway that is not answering is not asked a
    // question: neither the one whose list read failed nor the one that already ate a
    // whole search deadline in silence. Both still count as ASKED and answer at once as
    // unreached — dropping them silently would make the search look complete when part
    // of the fleet was never read (see `searchFanout`).
    const fanout = searchFanout(
      connsRef.current,
      machinesRef.current,
      searchFilter.machine,
      searchSilentRef.current,
    );
    setSearchAnswers({
      needle,
      scopeKey: searchFilter.key,
      asked: fanout.asked.filter(searchFilter.accepts),
      byMachine: new Map(fanout.dark.filter(searchFilter.accepts).map((key) => [key, UNREACHED])),
    });
    const reachable = fanout.ask.filter((conn) => searchFilter.accepts(machineKey(conn)));
    if (reachable.length === 0) return;
    const controller = new AbortController();
    searchControllerRef.current = controller;
    const answer = (key: string, entry: SearchAnswer) =>
      setSearchAnswers((prev) =>
        prev.needle === needle && prev.scopeKey === searchFilter.key
          ? { ...prev, byMachine: new Map(prev.byMachine).set(key, entry) }
          : prev,
      );
    for (const conn of reachable) {
      const key = machineKey(conn);
      const api = clientFor(conn);
      void (async () => {
        const found = await readSearchPage(api, needle, controller.signal, searchFilter.request(key));
        if (controller.signal.aborted) return;
        if (found === null) {
          // NOW KNOWN DARK. The next query skips this machine outright instead of
          // spending another `SEARCH_REACH_MS` rediscovering the same silence; its
          // next answered list read takes the mark off again (see `loadMachine`).
          searchSilentRef.current.add(key);
          answer(key, UNREACHED);
          return;
        }
        // The rows ride IN the answer, in the gateway's own order: a hit the paged list
        // has not loaded needs no second read.
        answer(key, {
          matches: found.matches, rows: found.sessions, reached: true,
          total: found.total, nextCursor: found.nextCursor,
        });
      })();
    }
    return () => {
      controller.abort();
      if (searchControllerRef.current === controller) searchControllerRef.current = null;
    };
  }, [searchQuestion, fleetKey, scope, searchFilter]);

  // WHAT IS TYPED VS WHAT WAS ASKED. `typed` is the field this frame; `searchNeedle` is
  // the needle every row, count and answer below belongs to. They differ only inside a
  // pause, and that difference is exactly what the row spends saying "searching..." —
  // the alternative was to re-filter, re-rank and re-count on every character, which is
  // the list jumping under the thumb a letter at a time.
  const typed = query.trim();
  const searching = typed.length > 0;
  // A NEEDLE HAS ACTUALLY BEEN ASKED, so the tally below counts a filtered list. Before
  // the first pause settles there is a query in the field and no question on the wire,
  // and a count then would be counting every session on the machine.
  const searched = searchNeedle.length > 0;
  // ONLY the answers to the needle on screen count. Anything filed under an older
  // needle is a superseded round trip, not a result.
  const live = searchAnswers.needle === searchNeedle && searchAnswers.scopeKey === searchFilter.key ? searchAnswers : null;
  const searchAsked = live?.asked ?? NO_MACHINES;
  const searchPages = [...(live?.byMachine.values() ?? [])];
  const searchHasMore = searchPages.some((answer) => answer.nextCursor !== null);
  const searchPaging = searchPages.some((answer) => answer.paging);
  const searchPageError = searchPages.some((answer) => answer.pageError);
  const searchTotal = searchPages.reduce((total, answer) => total + answer.total, 0);
  const loadMoreSearch = () => {
    const controller = searchControllerRef.current;
    if (!live || !controller || controller.signal.aborted || live.needle !== searchQuestion) return;
    for (const conn of connsRef.current) {
      const key = machineKey(conn);
      const entry = live.byMachine.get(key);
      if (!entry?.nextCursor || entry.paging) continue;
      const cursor = entry.nextCursor;
      const flight = JSON.stringify([live.scopeKey, live.needle, key, cursor]);
      if (searchPagesRef.current.has(flight)) continue;
      searchPagesRef.current.add(flight);
      const update = (change: (previous: SearchAnswer) => SearchAnswer) => {
        setSearchAnswers((previous) => {
          const answer = previous.byMachine.get(key);
          if (controller.signal.aborted || previous.needle !== live.needle ||
              previous.scopeKey !== live.scopeKey || answer?.nextCursor !== cursor) return previous;
          return { ...previous, byMachine: new Map(previous.byMachine).set(key, change(answer)) };
        });
      };
      update((previous) => ({ ...previous, paging: true, pageError: false }));
      void (async () => {
        try {
          const page = await readSearchPage(clientFor(conn), live.needle ?? '', controller.signal, {
            ...searchFilter.request(key), after: cursor,
          });
          if (page === null) {
            update((previous) => ({ ...previous, paging: false, pageError: true }));
            return;
          }
          update((previous) => ({
            ...previous,
            rows: [...new Map([...previous.rows, ...page.sessions].map((row) => [row.id, row])).values()],
            matches: [...new Map([...previous.matches, ...page.matches].map((match) => [match.sessionId, match])).values()],
            total: page.total, nextCursor: page.nextCursor, paging: false, pageError: false,
          }));
        } finally {
          searchPagesRef.current.delete(flight);
        }
      })();
    }
  };
  const searchAnswered = useMemo(
    () => new Set(searchAsked.filter((key) => live?.byMachine.has(key) === true)),
    [live, searchAsked],
  );
  // STILL ASKING — the field is ahead of the needle the list answers (inside the pause),
  // no question has been filed yet, or a machine that was asked has yet to come back.
  // The recents are asked the same way. This is the one fact the screen owed the reader
  // and did not have.
  const searchPending =
    isSearchOpen &&
    (typed !== searchNeedle || live === null || searchAnswered.size < searchAsked.length);
  // The machines that were ASKED and never answered — dark before the question was put,
  // or silent past `SEARCH_REACH_MS`. Kept apart from the ones that answered with
  // nothing, because "I looked and found nothing" and "you never heard from me" are
  // different facts and only the first one is a result.
  const searchUnreached = useMemo(
    () => new Set(searchAsked.filter((key) => live?.byMachine.get(key)?.reached === false)),
    [live, searchAsked],
  );
  // What an empty list is ALLOWED to say once the search has settled. A fleet that did
  // not answer has not looked, so "nothing matches that" would be a verdict nobody
  // reached — the same lie the in-flight case used to tell, one round trip later.
  const searchVerdict =
    searchUnreached.size === 0
      ? 'Nothing on any paired machine matches that.'
      : searchUnreached.size < searchAsked.length
        ? `Nothing on the machines that answered; ${searchUnreached.size} could not be reached.`
        : searchAsked.length > 1
          ? 'No machine answered.'
          : 'This machine did not answer.';
  const matches = useMemo(() => {
    if (!live) return null;
    const byId = new Map<string, SessionMatch>();
    for (const entry of live.byMachine.values())
      for (const match of entry.matches) byId.set(match.sessionId, match);
    return byId;
  }, [live]);
  // The rows each machine's answer carried, per machine key, in the gateway's own order:
  // the recents, or what the query matched. Kept beside the list instead of merged into
  // it: the 10s poll rewrites `machine.sessions` from the gateway's own paged answer.
  const searchRows = useMemo(() => {
    const byMachine = new Map<string, Session[]>();
    if (live) for (const [key, entry] of live.byMachine) byMachine.set(key, entry.rows);
    return byMachine;
  }, [live]);

  const inScope = useMemo(() => scopedMachines(machines, scope), [machines, scope]);

  // What there is to paint, which is not the same question as whether the gateway has
  // spoken. Saved rows go up in the first frame, so a cold start opens on the picture it
  // closed on (`SessionsScreen.paging.test`, `SessionsScreen.ordering.test`), and only a
  // scope with no rows at all and nothing answered yet has nothing to show. `null` is
  // that state: the skeleton stands in for rows nobody has, never for rows nobody has
  // confirmed yet. A scope that is no longer reading has its answer, empty or not.
  const sessions = useMemo(() => {
    if (machines.length === 0) return [];
    const rows = inScope.flatMap((machine) => machine.sessions ?? []);
    return inScope.some((machine) => machine.sessions !== null) || fleetState !== 'reading'
      ? rows
      : null;
  }, [inScope, machines.length, fleetState]);

  // ROWS SKIP LAYOUT OFF SCREEN ONLY AFTER ONE FULL LAYOUT, AND ONLY WHERE THE ENGINE
  // DRAWS AHEAD (see `SessionRow`). Every row records its real height on its first layout,
  // so `data-rows-settled` can turn on `content-visibility:auto` without a scroll restore
  // measuring placeholders. Chromium keeps about a screen of rows drawn past this
  // scroller's edges. WebKit draws only the rows inside it: on an iOS 18 simulator, fast
  // flings then showed more frames with rows not yet drawn at the leading edge, for 5-10 ms
  // saved on the way back. So the flag is tried on a row half a screen below the fold and
  // comes off again when that row is still skipped. A list too short to hold such a row
  // waits for more rows: another machine's sessions or an opened group. The flag is DOM
  // state, so it renders nothing.
  const isListShown = sessions !== null;
  useEffect(() => {
    const viewport = listRef.current;
    if (!isListShown || !viewport) return;
    let frame = 0;
    const afterTwoFrames = (then: () => void) => {
      cancelAnimationFrame(frame);
      frame = requestAnimationFrame(() => {
        frame = requestAnimationFrame(then);
      });
    };
    const settle = () => {
      const edge = viewport.getBoundingClientRect().bottom + viewport.clientHeight / 2;
      const probe = Array.from(viewport.querySelectorAll<HTMLElement>('[data-session-row]')).find(
        (row) => row.getBoundingClientRect().top >= edge,
      );
      if (!probe) return;
      rowsAdded.disconnect();
      viewport.setAttribute('data-rows-settled', '');
      afterTwoFrames(() => {
        if (!probe.firstElementChild?.checkVisibility?.({ contentVisibilityAuto: true })) {
          viewport.removeAttribute('data-rows-settled');
        }
      });
    };
    const rowsAdded = new MutationObserver(() => afterTwoFrames(settle));
    rowsAdded.observe(viewport, { childList: true, subtree: true });
    afterTwoFrames(settle);
    return () => {
      rowsAdded.disconnect();
      cancelAnimationFrame(frame);
      viewport.removeAttribute('data-rows-settled');
    };
  }, [isListShown]);

  // THE LIST NEVER FILTERS. It shows every session on each machine in scope, in the
  // gateway's order; a search answers in its own dialog (`SessionSearchDialog`), so the
  // list's rows, scroll position and open projects stay as the reader left them.
  const listed = useMemo(
    () =>
      inScope.map((machine) => ({
        machine,
        sessions: (machine.sessions ?? []).filter(
          (session) => !clientFor(machine.conn).isSessionDeleted(session.id),
        ),
      })),
    [inScope],
  );

  // The row the transcript beside this list belongs to, named the way a row names
  // itself. A STRING in the row context rather than the connection it came from:
  // that context is memoised, and an object would re-render every row per paint.
  const openRow = openSession ? sessionRowKey(openSession.conn, openSession.sid) : null;
  const openSid = openSession?.sid ?? null;

  // WHAT THE SEARCH FOUND is each machine's own answer: the recents while the field is
  // blank, else the sessions the query matched, both in the gateway's order. Filtering
  // happens INSIDE each machine: two checkouts of the same repo on two machines are two
  // projects, and a folder name never merges them.
  //
  // THE SESSION IN USE LEADS ITS MACHINE'S ANSWER, as the terminal switcher keeps the
  // session it was opened from on its first row. Regression, user report (paraphrased:
  // the search should start on the session I am in, not on the first result): the
  // dialog started on the freshest row, and the open session could sit pages down or
  // be missing from the recents altogether.
  const found = useMemo(() => {
    const needle = searchNeedle.toLowerCase();
    return inScope.filter((machine) => searchFilter.accepts(machineKey(machine.conn))).map((machine) => {
      const api = clientFor(machine.conn);
      const answered = (searchRows.get(machineKey(machine.conn)) ?? []).filter(
        (session) => !api.isSessionDeleted(session.id),
      );
      const isOpen = (session: Session) => sessionRowKey(machine.conn, session.id) === openRow;
      // The recents are painted as the gateway answered them: freshest first. Newer work
      // can push the open session out of their window; the row this device holds for it
      // stands in, because the session in use is recent work.
      if (!needle) {
        const open =
          answered.find(isOpen) ??
          (openSid !== null &&
          sessionRowKey(machine.conn, openSid) === openRow &&
          !api.isSessionDeleted(openSid)
            ? (machine.sessions?.find(isOpen) ?? api.cachedSession(openSid))
            : null);
        return { machine, sessions: openFirst(answered, open && searchFilter.includes(machine.conn, open) ? open : null) };
      }
      const draftFor = (session: Session) => draftMessages[draftMessageKey(api.base, session.id)];
      // The one thing no gateway can match: the words and file names waiting in THIS
      // device's composer. A loaded row whose unsent draft holds the query joins the
      // answer, behind it.
      const answeredIds = new Set(answered.map((session) => session.id));
      const drafted = (machine.sessions ?? []).filter(
        (session) =>
          !answeredIds.has(session.id) &&
          !api.isSessionDeleted(session.id) &&
          searchFilter.includes(machine.conn, session) &&
          draftSearchText(draftFor(session)).includes(needle),
      );
      const ordered = sessionOrder([...answered, ...drafted], {
        favoriteRank,
        hasDraftMessage: (session) => draftMessageHasUnsent(draftFor(session)),
      });
      // A query that matched the session in use answers with it on top.
      return { machine, sessions: openFirst(ordered, ordered.find(isOpen)) };
    });
  }, [inScope, searchNeedle, searchRows, draftMessages, openRow, openSid, searchFilter]);

  // A search is a FLEET question: it runs on every machine in scope, so the dialog
  // reports what came back and from how many of them.
  const searchCounts = useMemo(() => searchTally(found), [found]);

  // Apply every canonical response immediately, including live and recent arrivals.
  const visible = useMemo(
    () => (sessions === null ? null : listed.flatMap((entry) => entry.sessions)),
    [listed, sessions],
  );

  // A visit remains read after the pane closes, even if a paged row or a fleet read
  // still carries the old NEW. Later answers are counted above that visit's floor.
  const isRowUnread = useCallback(
    (conn: GatewayConn, session: Session) =>
      sessionRowKey(conn, session.id) !== openRow &&
      unreadAfterVisit(session, readFloors.get(sessionRowKey(conn, session.id))) > 0,
    [openRow, readFloors],
  );
  // A row read to its newest answer HERE: open beside the list, or visited since. The
  // gateway keeps counting it as new until its read mark returns with the next overview.
  const isRowSeen = useCallback(
    (conn: GatewayConn, session: Session) => {
      const key = sessionRowKey(conn, session.id);
      return key === openRow || (readFloors.get(key) ?? -1) >= answeredTurnCount(session);
    },
    [openRow, readFloors],
  );
  // Per-machine tallies for the strip and the machine headers.
  const tallies = useMemo(
    () =>
      new Map(
        machines.map((machine) => [
          machineKey(machine.conn),
          machineCounts(
            machine,
            sessionIsLive,
            (session) => isRowUnread(machine.conn, session),
            readSinceCounted(machine, (session) => isRowSeen(machine.conn, session)),
          ),
        ]),
      ),
    [machines, isRowUnread, isRowSeen],
  );
  const scopeMachine = scope
    ? (machines.find((machine) => machineKey(machine.conn) === scope) ?? null)
    : null;

  // Connectivity never changes positions. The app supplies the server's cached order.
  const switcherMachines = useMemo(() => {
    const ordered = [...machines];
    const primaryIndex = primaryKey
      ? ordered.findIndex((machine) => machineKey(machine.conn) === primaryKey)
      : -1;
    if (primaryIndex > 0) ordered.unshift(...ordered.splice(primaryIndex, 1));
    return ordered;
  }, [machines, primaryKey]);

  const selectScope = useCallback((next: string | null) => setScopePick(next), []);

  // ONE sheet, opened from wherever the machine is named: the row above the card when
  // the list is scoped, and that machine's own band in the fleet view. It is anchored
  // on the button that was pressed, so the anchor travels with the verb.
  const openManageProjects = useCallback((machine: FleetMachine, anchor: HTMLElement) => {
    const at = menuPosition(anchor.getBoundingClientRect(), BROWSE_WIDTH);
    if (!at) return;
    setManageProjects({ machine, at });
  }, []);

  const createSession = useCallback(
    // `groupId`: the reader asked on a GROUP's band, so the session is minted inside
    // that group and the new row appears at the top of the band they started it in.
    async (on: GatewayConn, root: string, groupId?: string) => {
      setCreating({
        at: creationKey(clientFor(on).base, root, groupId),
        label: 'Creating...',
      });
      setCreateError(null);
      try {
        const session = await clientFor(on).createSession({ root, groupId });
        // Open before refreshing the fleet. The full list walk is background work,
        // while the session the reader just requested is their immediate destination.
        if (session.id) await onOpen(on, session.id, true);
        void load();
      } catch (cause) {
        setCreateError((cause as Error).message);
      } finally {
        setCreating(null);
      }
    },
    [load, onOpen],
  );

  /** Copy the whole conversation and open the fork without waiting for the fleet read. */
  const forkSession = useCallback(
    async (session: Session, conn: GatewayConn) => {
      const forked = await clientFor(conn).forkSession(session.id);
      if (forked.id) await onOpen(conn, forked.id, true);
      void load();
    },
    [load, onOpen],
  );

  // The unit is the group ON THIS MACHINE, never "this project everywhere": the same
  // repo checked out on two machines is two projects and two deletes. A saved project is
  // one gateway request; a root-only group keeps the existing complete, best-effort walk.
  const removeManagedProject = useCallback(
    async (
      project: ManagedProject,
      conn: GatewayConn,
      onProgress: (progress: { done: number; total: number }) => void,
    ) => {
      const api = clientFor(conn);
      if (project.projectId) {
        const deleted = await api.deleteProject(project.projectId);
        forgetSessions(conn, deleted, project);
        return;
      }

      const ids = await projectSessionIds(api, project.root);
      const gone: string[] = [];
      let failed = 0;
      onProgress({ done: 0, total: ids.length });
      for (const sid of ids) {
        try {
          await api.deleteSession(sid);
          gone.push(sid);
        } catch {
          failed += 1;
        }
        onProgress({ done: gone.length + failed, total: ids.length });
      }
      // A partial fan-out is exactly known too: successful ids leave while refusals keep
      // their rows. Only a complete answer removes the project itself from the overview.
      forgetSessions(conn, gone, failed === 0 ? project : undefined);
      if (failed > 0) throw new Error(`${failed} of ${ids.length} sessions could not be deleted.`);
    },
    [forgetSessions],
  );

  const startDelete = useCallback((session: Session, conn: GatewayConn) => {
    setRowAction({ mode: 'delete', session, conn });
    setActionError(null);
  }, []);

  // Dismissable even mid-request. A delete already on the wire cannot be taken back, but
  // the row must never trap the screen for the full timeout of an unreachable machine.
  const cancelDelete = useCallback(() => {
    setRowAction(null);
    setActionError(null);
  }, []);

  const renameSession = useCallback(
    async (session: Session, conn: GatewayConn, title: string) => {
      const api = clientFor(conn);
      const key = machineKey(conn);
      // The gateway echoes the row it stored, so the new name arrives WITH the answer.
      // Ordering stays untouched: a row that jumps from under the thumb the instant it
      // is named reads as a bug, and the poll re-ranks it soon enough.
      const sid = session.id;
      const renamed = await api.renameSession(sid, title);
      patchMachine(key, (machine) => {
        const rows = machine.sessions;
        if (!rows || !rows.some((row) => row.id === sid)) return machine;
        return {
          ...machine,
          sessions: rows.map((row) =>
            row.id === sid ? { ...row, title, ...renamed, id: sid } : row,
          ),
        };
      });
    },
    [patchMachine],
  );

  const archiveSession = useCallback(
    async (session: Session, conn: GatewayConn, away: boolean) => {
      const sid = session.id;
      // The gateway stamps the row and echoes it back, so the band that paints the
      // other side of the archive lets the row go on the next paint, without a refetch.
      const moved = await clientFor(conn).setSessionArchived(sid, away);
      patchMachine(machineKey(conn), (machine) => {
        const rows = machine.sessions;
        if (!rows || !rows.some((row) => row.id === sid)) return machine;
        return {
          ...machine,
          sessions: rows.map((row) =>
            row.id === sid ? { ...row, ...moved, id: sid } : row,
          ),
        };
      });
      return moved;
    },
    [patchMachine],
  );

  async function commitDelete() {
    if (rowAction?.mode !== 'delete') return;
    const action = rowAction;
    setActionBusy(true);
    setActionError(null);
    try {
      // Regression, user report: deleting one session used to end in `load()`, a full
      // walk of every paired machine. The DELETE already names the one row to forget.
      await clientFor(action.conn).deleteSession(action.session.id);
      forgetSessions(action.conn, [action.session.id]);
      setRowAction((current) => (current === action ? null : current));
    } catch (cause) {
      setActionError((cause as Error).message);
    } finally {
      setActionBusy(false);
    }
  }

  // Deleting ONE session is confirmed IN the row, so the confirm has to reach
  // `commitDelete` from inside a memoised row. Through a ref, not a fresh
  // closure per paint: that would re-render every row of a 700-row list on
  // every poll.
  const commitRef = useRef<() => void>(() => {});
  commitRef.current = () => void commitDelete();
  const confirmDelete = useCallback(() => commitRef.current(), []);
  const deleting = rowAction?.mode === 'delete' ? rowAction : null;
  const rowCommands = useMemo<SessionRowCommands>(
    () => ({
      open: onOpen,
      read: noteOpenedRead,
      warm: warmTranscript,
      rename: renameSession,
      fork: forkSession,
      archive: archiveSession,
      requestDelete: startDelete,
      toggleStar,
    }),
    [onOpen, noteOpenedRead, renameSession, forkSession, archiveSession, startDelete, toggleStar],
  );
  const rowActions = useMemo<SessionListActions>(
    () => ({
      commands: rowCommands,
      deletion: {
        target: deleting,
        isBusy: actionBusy,
        error: actionError,
        confirm: confirmDelete,
        cancel: cancelDelete,
      },
    }),
    [rowCommands, deleting, actionBusy, actionError, confirmDelete, cancelDelete],
  );
  const projectCreation = useMemo<ProjectCreation>(
    () => ({ state: creating, start: createSession }),
    [creating, createSession],
  );

  const pageSize = useSessionsPerPage();

  // The list builds its groups from the gateway's project overviews, and the search dialog
  // builds them from the complete local match set. Both stay inside their own machine.
  const sections = useMemo(
    () =>
      listed.map((entry) => ({
        machine: entry.machine,
        // Carry page agreement with each machine entry.
        reading: {
          pageSize,
          isVisible,
          revision: machineMoves.get(machineKey(entry.machine.conn)) ?? 0,
          reads: machineReads.get(machineKey(entry.machine.conn)) ?? 0,
        },
        // Keep canonical gateway paths for identity and creation; shorten only for paint.
        groups: projectGroups(
          entry.machine.overview,
          entry.sessions,
          (session) => isRowUnread(entry.machine.conn, session),
          readSinceCounted(entry.machine, (session) => isRowSeen(entry.machine.conn, session)),
        ),
      })),
    [listed, pageSize, isVisible, isRowUnread, isRowSeen, machineMoves, machineReads],
  );
  const foundSections = useMemo(
    () =>
      found.map((entry) => {
        const groups = searchGroups(entry.sessions, (session) =>
          isRowUnread(entry.machine.conn, session),
        );
        // The session in use leads its machine's answer, and its project leads the
        // projects, as in the terminal switcher: the row the search starts on is in view.
        const open = groups.findIndex((group) =>
          group.sessions.some((session) => sessionRowKey(entry.machine.conn, session.id) === openRow),
        );
        return {
          machine: entry.machine,
          searchSessions: entry.sessions,
          // An open dialog is on the glass, whatever the list behind it is doing.
          reading: {
            pageSize,
            isVisible: true,
          },
          groups: open > 0 ? [groups[open], ...groups.filter((_, index) => index !== open)] : groups,
        };
      }),
    [found, pageSize, isRowUnread, openRow],
  );

  // THE ROW THE SEARCH PANE SHOWS: the one the reader last pressed while it is still a
  // result, else the session in use when the list holds it, else the first row. The recents
  // have the pane too, because the dialog is always split, as the terminal switcher is. The
  // pane is never empty beside a list, and it starts where the switcher's cursor does: on
  // the session the reader is in.
  const [previewPick, setPreviewPick] = useState<string | null>(null);
  if (!searching && previewPick !== null) setPreviewPick(null);
  const preview = useMemo(() => {
    let open: { session: Session; conn: GatewayConn } | null = null;
    let first: { session: Session; conn: GatewayConn } | null = null;
    for (const { machine, groups } of foundSections) {
      for (const group of groups) {
        for (const session of group.sessions) {
          if (session.id === previewPick) return { session, conn: machine.conn };
          if (sessionRowKey(machine.conn, session.id) === openRow) open ??= { session, conn: machine.conn };
          first ??= { session, conn: machine.conn };
        }
      }
    }
    return open ?? first;
  }, [foundSections, previewPick, openRow]);
  const previewId = preview?.session.id ?? null;
  const openPreview = useCallback(() => {
    if (!preview) return;
    rowCommands.read?.(preview.conn, preview.session);
    void rowCommands.open(preview.conn, preview.session.id);
  }, [preview, rowCommands]);
  // The list's rows answer no query: the search has its own rows, in its own dialog.
  const rowContext = useMemo<SessionRowsContext>(
    () => ({
      getClient: clientFor,
      drafts: draftMessages,
      needle: '',
      actions: rowActions,
      openRow,
      readFloors,
      previewId: null,
      preview: null,
    }),
    [draftMessages, rowActions, openRow, readFloors],
  );
  const foundContext = useMemo<SessionRowsContext>(
    () => ({
      ...rowContext,
      needle: searchNeedle,
      previewId,
      preview: searching ? setPreviewPick : null,
    }),
    [rowContext, searchNeedle, previewId, searching],
  );

  // Project management uses gateway overview counts, matching the visible headers.
  const managedProjects = useCallback(
    (machine: FleetMachine): ManagedProject[] =>
      projectGroups(machine.overview, machine.sessions ?? []).map((group) => ({
        name: group.label,
        root: group.root,
        projectId: group.projectId,
        count: group.tally.count,
        live: group.tally.live,
      })),
    [],
  );

  // Report only reachability transitions, and only a total fleet outage. This prevents
  // a dead-gateway mount loop while allowing degraded multi-machine lists.
  const loadError = fleetError(machines);
  const reportedError = useRef<string | null | undefined>(undefined);
  useEffect(() => {
    if (reportedError.current === undefined && loadError === null) {
      reportedError.current = null;
      return;
    }
    if (reportedError.current === loadError) return;
    reportedError.current = loadError;
    onUnreachable?.(loadError);
  }, [loadError, onUnreachable]);

  // THE DESK STANDS THIS LIST IN A SIDEBAR. `App` puts it in a 20rem column beside the
  // transcript — the list is always there, the conversation fills the rest — so what
  // the phone paints is what the desk paints: the machine switch on top, the projects
  // under it, flush to the column's edges. The page inset, the second fleet index that
  // stood beside a 1400px table and the table's own fixed columns all went with it; a
  // row answers its width to the list it is in (`@container`), not to the window.
  const isDesk = useDeskRail();

  const showStrip = machines.length > 0;

  // The footer's count is the GATEWAY's own (`machineCounts`), never a count of the
  // rows this device holds.
  const fleetSessions = machines.reduce(
    (total, machine) => total + (tallies.get(machineKey(machine.conn))?.sessions ?? 0),
    0,
  );

  // ONE VERB, ONE PLACE: the strip beside the switch, on the row that names the
  // machine it opens the projects of (`ui.test.tsx` counts the call site).
  const projectsVerb =
    scopeMachine && !scopeMachine.error ? (
      <MachineProjectsButton
        machine={machineLabel(scopeMachine.conn)}
        onPress={(anchor) => openManageProjects(scopeMachine, anchor)}
      />
    ) : null;
  if (loadError) return null;

  // ONE MACHINE SWITCH over the list. The search dialog offers the same choice as a
  // picker (`searchMachine`), because a search asks the machine the switch has picked.
  const machineSwitch = (
    <div role="group" aria-label="Machines" className="flex min-w-0 flex-1">
      <MachineSwitcher>
        {/* The machine tabs are the groups, and exactly one is always active. */}
        {switcherMachines.map((machine) => {
          const key = machineKey(machine.conn);
          const tally = tallies.get(key);
          const name = machineLabel(machine.conn);
          // A machine that is not answering cannot scope the screen to stale rows.
          // Keep its name in place; its error-toned tile retries the connection,
          // and the transport reason remains available in the title. Everything else
          // here is `MachineRead`'s rule, which the footer and the sections read too:
          // only a failure measured in this run earns the retry, and a machine with
          // cached rows or a remembered outage is being connected to right now.
          // Cached unread activity may still tint it.
          const read = machineRead(machine);
          const isDown = read === 'down';
          const isChecking = read === 'reading';
          const retry = isDown ? retries.get(key) : undefined;
          return (
            <MachineTab
              key={key}
              isOn={scope === key}
              hasUnread={!isDown && (tally?.unread ?? 0) > 0}
              isDown={isDown}
              // WHILE CONNECTING, SAY SO — in the tile, where a finger is. The pending
              // state used to live in `title=` alone, an attribute that does not exist on
              // touch, so a phone showed a bare machine name for as long as the read took.
              note={
                retry === 'busy'
                  ? 'Reconnecting…'
                  : retry === 'failed'
                    ? 'Unable to connect'
                    : isChecking
                      ? 'Connecting…'
                      : null
              }
              // Only the FAILURE is error ink: connecting is not a failure.
              isNoteError={retry === 'failed'}
              label={isDown ? `Reconnect to ${name}` : undefined}
              title={
                isDown
                  ? `${name} is not answering — ${machine.error}`
                  : isChecking
                    ? `Checking ${name}…`
                    : undefined
              }
              onClick={() => (isDown ? void retryMachine(machine.conn) : selectScope(key))}
            >
              {name}
            </MachineTab>
          );
        })}
      </MachineSwitcher>
    </div>
  );
  // The search asks the machine the switch has picked. With more than one machine, the
  // dialog offers that choice as a picker beside its project and groups. A machine that
  // is not answering has nothing to search: it stays listed there, but cannot be chosen.
  const searchMachine =
    switcherMachines.length > 1
      ? {
          value: scope ?? '',
          options: switcherMachines.map((machine) => {
            const read = machineRead(machine);
            const note = read === 'down' ? 'Not answering' : read === 'reading' ? 'Connecting…' : null;
            const name = machineLabel(machine.conn);
            return {
              value: machineKey(machine.conn),
              label: note ? `${name} · ${note}` : name,
              disabled: read === 'down',
            };
          }),
          onChange: selectScope,
        }
      : null;
  // What the search came back with, on the line under its choices.
  const searchReport = searching && sessions !== null && (
    <div className="flex min-w-0 flex-wrap items-center gap-x-2 gap-y-1">
      {/* A filter is a FLEET question, and the count it came back with is the
          only proof it left this gateway. It is the one fact this row reports:
          totals were the same numbers the project headers below already carry.
          It counts the ROWS ON SCREEN, so it is spoken only once a needle has
          actually been asked: inside a pause the list is still the last answer,
          and before the first one there is nothing filtered to count. While the gateway
          holds more pages, it says how many of the scope's total those rows are. */}
      {searched && (
        <span className="whitespace-nowrap font-mono text-chip font-bold text-accent-ink">
          {searchHasMore && searchTotal > searchCounts.matches
            ? `${searchCounts.matches} of ${searchTotal} matches`
            : `${searchCounts.matches} ${searchCounts.matches === 1 ? 'match' : 'matches'}`}
        </span>
      )}
      {/* A search is a fleet ROUND TRIP over a transcript store, not a filter
          over rows already here, so it has a DURATION and the row has to spend
          it saying so — the report was waiting with nothing on screen but a
          count that was really just "nothing yet". The same slot therefore
          reports PROGRESS while machines are still reading ("searching 1 of 3
          machines...") and the shipped tally the moment they have all answered;
          the count beside it grows as each one lands, because every machine
          paints the moment IT answers instead of behind the slowest. It is also
          what a PAUSE says: the field is ahead of the needle the list answers,
          and the honest word for that is the same one. */}
      {searchPending ? (
        <span
          aria-live="polite"
          className="whitespace-nowrap font-mono text-chip text-dialog-hint"
        >
          {searchAsked.length > 1
            ? `searching ${searchAnswered.size} of ${searchAsked.length} machines...`
            : 'searching...'}
        </span>
      ) : (
        <>
          {/* WHERE the query went, and only a fleet has an answer worth
              printing: "across 2 of 3 machines" is the proof it left this
              gateway. A solo user is told nothing they can act on. */}
          {inScope.length > 1 && (
            <span className="whitespace-nowrap font-mono text-chip text-dialog-hint">
              across {searchCounts.machines} of {inScope.length} machines
            </span>
          )}
          {/* A MACHINE THAT NEVER ANSWERED IS NOT A MACHINE THAT FOUND NOTHING,
              and only this row can tell the reader which one it was: the search
              covered less of the fleet than it was asked to, and every count
              beside this is short by that much. In failure ink, because it is
              the one part of the answer that did not arrive. */}
          {searchUnreached.size > 0 && (
            <span className="whitespace-nowrap font-mono text-chip text-err">
              {searchUnreached.size} {searchUnreached.size === 1 ? 'machine' : 'machines'}{' '}
              did not answer
            </span>
          )}
        </>
      )}
    </div>
  );
  // THE SEARCH'S OWN LIST. Before a query it lists the recent sessions, while machines
  // read their sessions or transcripts it says so, and once they all answered it says
  // what came back.
  const searchResults =
    sessions === null ? (
      <NavigatorSkeleton />
    ) : foundSections.every(({ groups }) => groups.length === 0) ? (
      <div className="px-5 py-16 text-center">
        {/* A query whose answer has not come back yet is not a dead end, and saying "No
            matching sessions" while every gateway is still reading its transcripts is the
            screen lying about a result it does not have. */}
        <p className="font-mono text-body font-bold text-white/70">
          {searchPending
            ? searching
              ? 'Searching...'
              : 'Reading recent sessions...'
            : searching
              ? 'No matching sessions'
              : 'No recent sessions'}
        </p>
        <p aria-live="polite" className="mt-2 font-mono text-ui text-dialog-hint">
          {searchPending
            ? searchAsked.length > 1
              ? `Read ${searchAnswered.size} of ${searchAsked.length} machines so far.`
              : searching
                ? 'Reading this machine’s transcripts.'
                : 'Reading this machine’s sessions.'
            : searching || searchUnreached.size > 0
              ? searchVerdict
              : 'Type a word from its title or messages.'}
        </p>
      </div>
    ) : (
      <>
        <MachineSections
          isSearch
          sections={foundSections}
          searchScope={searchScope}
          context={foundContext}
          creation={projectCreation}
          note={(machine) => {
            const key = machineKey(machine.conn);
            // Saved rows are not an answer here either (see `MachineRead`): the dialog
            // waits on the gateway, not on whatever this device kept from last time.
            if (machineRead(machine) === 'reading') return 'Reading sessions...';
            if (searchUnreached.has(key)) return 'Could not reach this machine.';
            if (!searchAnswered.has(key))
              return searching ? 'Searching this machine...' : 'Reading this machine...';
            return searching ? 'No matches on this machine.' : 'No recent sessions on this machine.';
          }}
        />
        {/* More results load where the list stops, as the reader scrolls there. */}
        {(searchHasMore || searchPaging || searchPageError) && (
          <SearchPageEnd isPaging={searchPaging} hasFailed={searchPageError} onReach={loadMoreSearch} />
        )}
      </>
    );

  return (
    <section
      aria-label="Sessions"
      className={`flex h-full min-h-0 w-full flex-col pl-[env(safe-area-inset-left)] pr-[env(safe-area-inset-right)] pt-0 transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none ${isDesk ? '' : 'mx-auto max-w-[1400px] sm:px-6 sm:py-4'}`}
    >
      {/* A pending share belongs to the whole fleet, not the selected machine. */}
      {share && (
        <div className={`shrink-0 px-3 pt-3 ${isDesk ? '' : 'sm:pb-3 sm:pl-0 sm:pr-4 sm:pt-0'}`}>
          <Banner
            kind="neutral"
            title="Sharing"
            dismiss={
              onDiscardShare ? { label: 'Discard the share', onClick: onDiscardShare } : undefined
            }
          >
            {shareSummary(share)} — pick a session, or start a new one
          </Banner>
        </div>
      )}
      {/* The switch stays visible even for a fleet of one: it names the machine
          that owns the projects below. A borderless folder icon stands at the trailing
          edge. */}
      {/* The phone and desk sidebar use equal 12px vertical insets. On wider
          standalone layouts, the section already supplies the top inset. */}
      {showStrip && (
        <div
          className={`relative z-10 flex flex-wrap items-center gap-x-2 gap-y-2 px-3 py-3 ${isDesk ? '' : 'sm:pl-0 sm:pr-4 sm:pt-0'}`}
        >
          {machineSwitch}
          <div className="flex shrink-0 items-center gap-2">
            {/* Only when no button can speak for it: a create started from this row's
                own menu belongs to no project header. Every header-started create
                wears its word INSIDE the button that was pressed. */}
            {creating && creating.at === null && (
              <span aria-live="polite" className="font-mono text-chip text-dialog-hint">
                {creating.label}
              </span>
            )}
            {/* Open the selected machine's projects from the named action beside it. */}
            {projectsVerb}
          </div>
        </div>
      )}
      {/* ON A PHONE THE CARD IS THE PAGE, AND IT DOES NOT BREATHE.
            It used to be `mx-3` with a full box and a height that followed its content,
            so every page of the pager resized the frame under the finger (page 74 has 1
            row) and the whole screen jumped; its two side rules also stole 12px of a
            390px glass for nothing. Full bleed, no vertical rules, and `h-full` keep the
            frame fixed while the rows scroll inside it. The final machine closes the list with
            one neutral 2px rule exactly where its content ends; machine identity stays in the
            switcher instead of becoming another frame around every section. The bottom safe
            area is the LIST's own padding, never the section's: a section inset stood
            under the home indicator as an opaque strip of paper that the rows stopped
            above, so the last row of a page sat cut in half behind it and no amount of
            scrolling could bring it out. Rows now scroll under the indicator, and the
            list's padding lifts the final row and its closing rule clear of it.
            At `sm` the card detaches from the viewport edges but still fills the
            available height. Its list owns overflow; the document never grows a second
            scrollbar or leaves an intrinsic-height strip above empty desktop paper. */}
      {/* AND IT IS NEVER A CARD ITSELF: it is the PAGE the project sheets stand on,
            on the glass exactly as on the desk. It keeps no frame — a container that
            holds objects with their own edges is not itself an object — and takes the
            derived page paper, one step under the sheet in either palette. THAT STEP
            IS WHAT A CORNER IS CUT OUT OF: for as long as this card carried the
            sheet's own paper on a phone, a round there would have cut paper out of
            the same paper, so the projects were square on the glass and sheets on the
            desk for no reason a reader could see. */}
      {/* Overlay the viewport edge so it stays visible while headers scroll underneath.
          Sharing the first header's pixel avoids a doubled rule or a layout shift. */}
      <div className="relative flex h-full min-h-0 flex-col overflow-hidden bg-page before:pointer-events-none before:absolute before:inset-x-0 before:top-0 before:z-20 before:border-t before:border-white sm:max-h-full">
        {/* The pull reports itself where the search door lives: it takes over the app bar
            until the finger releases, instead of inserting a new band above the list.

            IT HANGS IN THE APP'S OVERLAY LAYER, NOT IN THIS SCREEN. The band is pinned to
            the glass, which holds only while nothing above it is transformed — and the back
            swipe out of a session (`lib/edge-back`) drags this whole pane. A `fixed` element
            inside a transformed ancestor is pinned to THAT ancestor, so the band's resting
            place, one band height above the top, landed back on the glass under the app bar
            for the length of every stroke. */}
        {createPortal(<PullToSearchHint phase={pullPhase} ref={hintRef} />, overlayLayer().host)}
        {/* A create that failed has no button left to speak from once the order's own
            popover is gone, so the word lands on the paper the list is about to fill. */}
        {createError && (
          <div className="border-b border-dialog-edge bg-panel-2 px-3 py-2 sm:px-4">
            <Banner kind="err">{createError}</Banner>
          </div>
        )}

        <div
          ref={listRef}
          className={`@container min-h-0 flex-1 touch-pan-y overflow-x-hidden overflow-y-auto overscroll-contain [overflow-anchor:auto] [scrollbar-color:color-mix(in_srgb,var(--dialog-hint)_12%,transparent)_transparent] pb-[calc(0.75rem+env(safe-area-inset-bottom))] ${isDesk ? '[scrollbar-width:none] [&::-webkit-scrollbar]:hidden' : 'sm:px-3 [scrollbar-gutter:stable]'}`}
        >
          {sessions === null ? (
            <NavigatorSkeleton />
          ) : visible?.length === 0 && sections.every(({ groups }) => groups.length === 0) ? (
            <div className="px-5 py-16 text-center">
              {/* This state means there is NO PROJECT: an empty project still renders its
                own header and the New session action it owns. */}
              <p className="font-mono text-body font-bold text-white/70">No projects yet</p>
              <p className="mt-2 font-mono text-ui text-dialog-hint">
                Add a project to start a session.
              </p>
            </div>
          ) : (
            <MachineSections
              sections={sections}
              context={rowContext}
              creation={projectCreation}
              note={(machine) =>
                ({
                  reading: 'Reading sessions...',
                  // A machine this run could not reach has nothing to report about
                  // projects; its tile carries the reason and the retry.
                  down: 'This machine is not answering.',
                  settled: 'No projects on this machine yet.',
                })[machineRead(machine)]
              }
            />
          )}
        </div>

        {/* Only the WAIT is left here. The fraction moved into the filter band, which
            is where the filtering happens — printing "708 of 970" in a footer while
            the control that produced it said nothing was the same fact in the wrong
            place, and the third copy of it on the screen. */}
        {fleetState === 'reading' && (
          <footer className="hidden items-center justify-end border-t border-dialog-edge bg-panel-2 px-3 py-2 font-mono text-meta text-dialog-hint sm:flex sm:bg-page sm:px-4">
            <span>Reading sessions...</span>
          </footer>
        )}
        {/* THE DESK'S OWN FOOTER, and it says only what is true here: `Ctrl+/` opens the
            fleet search from anywhere, a field included (`App`), and the count is the
            gateway's. The sidebar has room for a footer without spending a row of the
            list on it. */}
        {isDesk && (
          <footer className="flex items-center justify-between gap-3 border-t border-dialog-edge bg-panel-2 px-3 py-1.5 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
            <span className="flex items-center gap-1.5">
              <kbd className="border border-dialog-edge px-1 font-mono text-chip normal-case">
                Ctrl+/
              </kbd>
              Search
            </span>
            <span className="tabular-nums">
              {machines.length > 1
                ? `${fleetSessions} sessions · ${machines.length} machines`
                : `${fleetSessions} ${fleetSessions === 1 ? 'session' : 'sessions'}`}
            </span>
          </footer>
        )}
      </div>

      {manageProjects && (
        <ManageProjectsSheet
          label={machineLabel(manageProjects.machine.conn)}
          at={manageProjects.at}
          client={clientFor(manageProjects.machine.conn)}
          startAt={machineProject(manageProjects.machine)?.path ?? null}
          knownRoots={
            new Set(
              projectGroups(manageProjects.machine.overview, manageProjects.machine.sessions ?? [])
                .map((group) => group.root)
                .filter(Boolean),
            )
          }
          projects={managedProjects(manageProjects.machine)}
          onCancel={() => setManageProjects(null)}
          onChoose={async (root: string) => {
            const conn = manageProjects.machine.conn;
            await clientFor(conn).ensureProject(root);
            await load();
            setManageProjects(null);
          }}
          onRemove={(entry, onProgress) =>
            removeManagedProject(entry, manageProjects.machine.conn, onProgress)
          }
        />
      )}
      {isSearchOpen && (
        <SessionSearchDialog
          query={query}
          onQuery={onQuery}
          onClose={onCloseSearch}
          scope={<SessionSearchScopes scope={searchScope} machine={searchMachine} report={searchReport} />}
          results={searchResults}
          messages={
            preview && (
              <SearchMessages
                title={preview.session.title?.trim() || 'Untitled session'}
                match={searching ? matches?.get(preview.session.id) ?? null : null}
                query={searching ? searchNeedle : ''}
                isSearching={searching && searchPending}
                onOpen={openPreview}
                className="min-h-0 flex-1"
              />
            )
          }
        />
      )}
    </section>
  );
}

/** `rows` led by `open`, the session in use, and otherwise in their own order. */
function openFirst(rows: Session[], open: Session | null | undefined): Session[] {
  return open ? [open, ...rows.filter((session) => session.id !== open.id)] : rows;
}

/**
 * THE END OF THE SEARCH RESULTS, WHERE THE NEXT PAGE LOADS.
 *
 * Reported: the reader had to press "Load more results" under the last row to see more
 * matches. A mark now stands under the last row, and it asks for the next page once it
 * comes within half a pane of view. Each answered page arms it again, so a page too short
 * to fill the pane asks for the one after it. A page that failed waits for Try again:
 * asking again on its own would repeat the failure in a loop.
 */
function SearchPageEnd({ isPaging, hasFailed, onReach }: {
  /** A next page is on its way. */
  isPaging: boolean;
  /** The last page could not be read. */
  hasFailed: boolean;
  /** Ask every machine that has more results for its next page. */
  onReach: () => void;
}) {
  const markRef = useRef<HTMLDivElement>(null);
  const reach = useRef(onReach);
  useEffect(() => {
    reach.current = onReach;
  });
  const isArmed = !isPaging && !hasFailed;
  useEffect(() => {
    const mark = markRef.current;
    if (!isArmed || !mark || typeof IntersectionObserver === 'undefined') return;
    const observer = new IntersectionObserver(
      (entries) => {
        if (entries.some((entry) => entry.isIntersecting)) reach.current();
      },
      { root: scrollerOf(mark), rootMargin: '0px 0px 50% 0px' },
    );
    observer.observe(mark);
    return () => observer.disconnect();
  }, [isArmed]);
  return (
    <div className="px-3 pb-3 sm:px-4">
      <div ref={markRef} aria-hidden="true" className="h-px" />
      {isPaging && <LoadMore label="Loading more results">Loading more results…</LoadMore>}
      {hasFailed && (
        <>
          <LoadMore label="Try loading more results again" tone="error" onClick={onReach}>
            Try again
          </LoadMore>
          <p role="status" className="mt-1 text-center font-mono text-meta text-err">
            Could not load more results. Your current results are kept.
          </p>
        </>
      )}
    </div>
  );
}

/** The nearest ancestor that scrolls `node`, or `null` when the page itself scrolls. */
function scrollerOf(node: HTMLElement): HTMLElement | null {
  for (let at = node.parentElement; at; at = at.parentElement) {
    if (/auto|scroll/.test(getComputedStyle(at).overflowY)) return at;
  }
  return null;
}

type MachineSection = {
  machine: FleetMachine;
  reading: ProjectGroupReading;
  groups: ProjectGroupView[];
  searchSessions?: Session[];
};

/**
 * One named section per machine: its projects, or the note that stands in for them.
 * The session list and the search dialog both stand their rows on it.
 */
function MachineSections({
  isSearch = false,
  sections,
  context,
  creation,
  searchScope,
  note,
}: {
  isSearch?: boolean;
  sections: MachineSection[];
  context: SessionRowsContext;
  creation: ProjectCreation;
  searchScope?: SearchScope;
  /** What a machine with no project to show says instead. */
  note: (machine: FleetMachine) => string;
}) {
  return (
    <div>
      {sections.map(({ machine, groups, reading, searchSessions }, sectionIndex) => {
        const key = machineKey(machine.conn);
        return (
          <section key={key} aria-label={`${machineLabel(machine.conn)} ${isSearch ? 'search results' : 'projects'}`}>
            {/* Every machine keeps its own named panel and landmark, even when it
              is the only one in the fleet: the landmark is a NAME, not ink. */}
            {/* Reported (paraphrased: bin that rail on the left): a machine's
              hue used to run 2px down everything it owned and close it with a
              rule, and with three machines paired that stripe was the full
              height of the glass. The reader picks a machine in the switch
              above this list, not by comparing rows 800px apart, so where one
              computer ends is the trough this gap opens and the name its
              landmark carries — the first project of the second machine can
              still never read as the fifth project of the first. */}
            {sectionIndex > 0 && <MachineGap />}
            {/* The active tab directly above the card already names this machine, so
              the list has no second selected/unselected presentation to maintain. */}
            {groups.length === 0 ? (
              <div className="px-3 py-3 sm:px-4">
                <p className="font-mono text-meta text-dialog-hint">{note(machine)}</p>
              </div>
            ) : isSearch && searchScope ? (
              <SearchSessionRows conn={machine.conn}
                sessions={searchSessions ?? []} context={context} scope={searchScope} />
            ) : (
              groups.map((group, groupIndex) => (
                // Nothing separates two projects: the band that opens the next
                // one brings its own paper and its own rule in over the name.
                <ProjectGroup
                  key={`${key}\u0000${group.root}`}
                  group={group}
                  machine={machine}
                  context={context}
                  reading={reading}
                  creation={creation}
                  // The order already put the machine's live work on top; the
                  // project it lands on is the one that opens by itself.
                  initiallyOpen={groupIndex === 0}
                />
              ))
            )}
          </section>
        );
      })}
    </div>
  );
}

/** How many rows one purge walk asks for at a time — a read nobody is watching. */
const PURGE_WALK = 200;

/** Walk gateway project pages to collect the complete session ID set for purge. */
async function projectSessionIds(api: GatewayClient, root: string): Promise<string[]> {
  const pins: ProjectWindows = new Map();
  const ids: string[] = [];
  let after = '';
  for (;;) {
    const page = await api.listProjectPage(root, PURGE_WALK, after, pins);
    for (const session of page.rows) ids.push(session.id);
    if (!page.nextCursor || page.rows.length === 0 || ids.length >= page.total) break;
    after = page.nextCursor;
  }
  return ids;
}

// How long the row's disclosure takes to open or close. It is duplicated by the
// `duration-200` utilities below on purpose: the class drives the paint, this
// number only decides when the panel may leave the tree.
function sessionViewFingerprint(session: Session): string {
  return JSON.stringify([
    session.id,
    session.title,
    session.status,
    session.live,
    session.current_turn_id,
    session.turn_count,
    session.answer_count,
    session.is_unread,
    session.unread_answers,
    session.was_interrupted,
    session.was_failed,
    session.modified_at,
    session.created_at,
    session.project_id,
    session.project_name,
    session.project_position,
    session.workspace?.root,
    session.workspace?.repo_root,
    session.workspace?.label,
  ]);
}

function reconcileSessions(current: Session[] | null, incoming: Session[]): Session[] {
  if (!current) return incoming;
  const previousById = new Map(current.map((session) => [session.id, session]));
  const next = incoming.map((session) => {
    const previous = previousById.get(session.id);
    return previous && sessionViewFingerprint(previous) === sessionViewFingerprint(session)
      ? previous
      : session;
  });
  return current.length === next.length &&
    current.every((session, index) => session === next[index])
    ? current
    : next;
}
