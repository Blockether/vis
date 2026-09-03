import { useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState } from 'react';
import { Banner, Button, LoadMore, Spinner } from '../components/ui';
import {
  MachineGap,
  MachineMark,
  MachineProjectsButton,
  MachineSwitcher,
  MachineTab,
  PullToSearchHint,
} from '../components/SessionNavigator';
import {
  NavigatorSkeleton,
  draftSearchText,
  firstLine,
  sessionSearchText,
  type SessionListActions,
  type SessionRowAction,
  type SessionRowCommands,
} from '../components/SessionList';
import {
  ProjectGroup,
  type ProjectCreation,
  type SessionRowsContext,
} from './sessions/SessionProjectGroups';
import { FleetRail } from './sessions/FleetRail';
import {
  Menu,
  MenuHeading,
  MenuItem,
  MenuNote,
  MENU_WIDTH,
  PANEL_SIZES,
} from '../components/Menu';
import { GatewayClient, type ProjectWindows, type SessionMatch } from '../lib/gateway';
import { SessionSubscriptionHub } from '../lib/subscriptions';
import type { ForkPoint, GatewayConn, Session, SseEvent } from '../lib/types';
import { VIEW_CLOSE_EVENT, VIEW_OPEN_EVENT, viewKind } from '../lib/view';
import { onWake } from '../lib/wake';
import { seedReadMarks, unreadTurnCount, useReadMarks } from '../lib/unread';
import { reassertBadge, syncBadge } from '../lib/badge';
import { assignMachineColors, machineColor } from '../lib/machine-colors';
import { menuPosition } from '../lib/anchored-menu';
import {
  applyListScroll,
  forgetListScroll,
  parkedListScroll,
  rowOffset,
  topVisibleRow,
  useListScrollPark,
  type ListAnchor,
} from '../lib/list-scroll';
import { EPOCH_STALE_AWAY_MS, holdOrder, useOrderEpoch } from '../lib/order-epoch';
import { usePullToSearch, type PullPhase } from '../lib/pull-to-search';
import {
  ManageProjectsSheet,
  type ManagedProject,
} from '../components/ManageProjectsSheet';
import { useDeskRail, useFitRows, useMouseDensity } from '../lib/fit-rows';
import {
  clearMachineOutage,
  machineOutage,
  rememberMachineOutage,
} from '../lib/fleet-outage';
import {
  clearDraftMessage,
  dirtySessionIds,
  draftMessageHasUnsent,
  draftMessageKey,
  flushDraftMessages,
  useDraftMessages,
} from '../lib/draft-messages';
import { shareSummary, type SharedPayload } from '../lib/share-intake';
import { favoriteRank, nextFavoriteRank } from '../lib/favorites';
import {
  fleetError,
  isFleetLoaded,
  machineCounts,
  machineKey,
  machineLabel,
  projectGroups,
  searchGroups,
  reconcileMachines,
  resolveScope,
  sameOverview,
  scopedMachines,
  SEARCH_UNPLACED,
  searchFanout,
  searchOrder,
  searchTally,
  sessionIsLive,
  sessionMillis,
  sessionOrder,
  withSearchHits,
  machineProject,
  type FleetMachine,
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
    (viewKind(event) === 'input' && (event.type === VIEW_OPEN_EVENT || event.type === VIEW_CLOSE_EVENT))
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

/** How many just-created sessions stay admitted past the held order at once. */
const MINTED_KEEP = 8;

// A query can name more sessions than a phone will ever scroll. Hydrating the
// unloaded hits is one GET each, so the tail is cut rather than paid for.
const SEARCH_HYDRATE_MAX = 40;

// Searching is a FLEET round trip — one ranked FTS query per paired machine, and on a
// large store the gateway spends ~130ms in SQLite before it answers. Firing that per
// keystroke would queue a search behind every letter of a word and leave the last one
// racing its own predecessors. This pause is what typing RESTING means: it gates the
// needle itself (`searchNeedle`), so it holds back the network AND every re-filter,
// re-rank and re-count the screen does — a keystroke costs the field alone.
const SEARCH_DEBOUNCE_MS = 200;

// A SEARCH IS A GESTURE, and a fleet is only ever as quiet as its quietest machine. The
// transport gives every request 30s (`REQUEST_TIMEOUT_MS`) — the right budget for a list
// read that pages, the wrong one for a question somebody is watching: a paired laptop
// that is asleep TAKES the socket without refusing it, so one dark machine held
// `searching 1 of 3 machines...` on screen for half a minute and then filed itself as
// having found nothing. A ranked FTS query over a 2.2 GB store answers in ~230ms, so
// silence this long is not slowness, it is absence — and absence is reported, not waited
// out.
const SEARCH_REACH_MS = 8_000;

// ONE machine's answer to the live query: the ranked hits it found, the sessions those
// hits named that this page had not paged in yet, and whether the machine ANSWERED AT
// ALL. `reached: false` is not an empty result — it is the absence of one, and the
// screen prints it as such.
type SearchAnswer = { matches: SessionMatch[]; rows: Session[]; reached: boolean };

// A machine that was asked and did not speak: dark before the question, or silent past
// `SEARCH_REACH_MS`.
const UNREACHED: SearchAnswer = { matches: [], rows: [], reached: false };

// The fleet's answer to ONE needle. `asked` is who the question went to, so
// `asked.length - byMachine.size` is exactly how much of the search is still
// outstanding — the progress the screen reports while it waits.
type SearchAnswers = { needle: string; asked: string[]; byMachine: Map<string, SearchAnswer> };

const NO_MACHINES: string[] = [];

const NO_SEARCH: SearchAnswers = { needle: '', asked: NO_MACHINES, byMachine: new Map() };

// A RETRY IS A GESTURE, so it answers on a gesture's clock. The transport gives every
// request 30s (`REQUEST_TIMEOUT_MS`) and a list read can page, so a tile pressed on a
// machine that is blackholed rather than refused — a closed laptop does not refuse a
// socket — wore `reconnecting...` for half a minute or more. Five seconds of silence IS
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

// MACHINES THIS DEVICE HAS ALREADY FOUND DARK. A machine that is not answering is drained
// out of `All` (`scopedMachines`) — but the fleet was rebuilt from nothing on every mount, and
// opening a session unmounts this screen, so a laptop that was asleep came back as a machine
// nobody had tried yet: its last known rows took a band, a rail and a section in the middle of
// the working fleet, and the probe behind them spent the transport's whole budget confirming
// what this device already knew before that section vanished under the reader.
//
// The verdict outlives the screen that reached it AND the run that measured it — it is SAVED
// (`lib/fleet-outage`), because the OS kills this webview whenever it feels like it and a
// machine that was off all night is not a machine nobody has met. A machine known to be dark
// starts drained, and walks back into the fleet when it ANSWERS — never because it is being
// asked again.

// ONE MISSED READ IS NOT AN OUTAGE, and this counts the failures nothing has confirmed
// yet.
//
// A machine that has been answering is a machine that is THERE: a phone hands its radio
// between cells, a Wi-Fi roam drops the socket mid-flight, and the read that runs the
// instant a session is left lands in exactly that gap. Publishing that one failure
// declared the machine dark — which drains it out of `All` AND drops the reader's scope
// back to the whole fleet (`resolveScope`), so leaving a transcript flipped the screen
// from the one machine being read to every machine, and back again a poll later. The
// darkness has to be CONFIRMED: a machine that has spoken to this device keeps its rows,
// its section and the scope through a single failed read, and the next read decides. A
// machine that has NOT spoken has nothing on screen to protect and goes dark on the
// first failure, exactly as before.
const fleetMisses = new Map<string, number>();

// Two failed reads in a row is the confirmation. The poll behind the first one is ten
// seconds away, so a gateway that really is gone still drains within a cycle.
const OUTAGE_CONFIRMING_MISSES = 2;

// Rebuild the fleet from the paired machines, painting each new one from its own
// last known list so a machine that was on screen a second ago comes back with
// rows instead of a skeleton — drained from the first frame when it is known dark.
function hydrateMachines(conns: GatewayConn[], previous: FleetMachine[]): FleetMachine[] {
  return reconcileMachines(conns, previous).map((machine) => {
    if (machine.error !== null) return machine;
    const outage = machineOutage(machineKey(machine.conn));
    // Cached rows are what to paint WHEN it answers — never a claim that it has.
    // A verdict this run has not measured is REMEMBERED: it drains the machine exactly as a
    // fresh failure does, but it is not this device WATCHING the fleet go dark (`fleetError`).
    if (machine.sessions !== null)
      return outage ? { ...machine, error: outage, isRemembered: true } : machine;
    const api = clientFor(machine.conn);
    // The header row's numbers come back with the machine, in the FIRST frame: a
    // gateway returned to must not re-tally itself out of session windows.
    const overview = machine.overview ?? api.cachedProjectsOverview();
    const cached = api.cachedSessions();
    if (!cached && !outage) return overview ? { ...machine, overview } : machine;
    return { ...machine, sessions: cached ?? null, error: outage, isRemembered: outage !== null, overview };
  });
}

// The scope strip's tabs live in `MachineSwitcher`/`MachineTab` (`components/ui`):
// one track, one raised tile for the machine you are on, no per-tab borders.


/**
 * The list's geometry in px, read off the live screen (dev server, Chromium, one
 * machine whose top project holds 102 pages of sessions):
 *
 *   - a session row is 48px with a 1px rule under it, and 32px + 1px under
 *     `mouse:`, where the density follows the pointer (`index.css`);
 *   - the first row of a project starts at y=211 on a phone, at y=149 under a
 *     pointer and at y=138 on a desk — the app bar, the filter row, the scope strip,
 *     the project's own band and the shelf carrying its pager, all of which a page
 *     pays for before its first row. Twelve of those pixels arrived late: the band is
 *     48px under a pointer rather than 36, because the path moved under the name and
 *     36 left the stack no air at all (`HEADER_BAND` in `components/ui`). The desk
 *     used to pay 215 for the same bands because the page's top inset was spelled
 *     TWICE, once on the section and once on the scope strip standing inside it;
 *   - FOOT is the gap the panel keeps under itself once it detaches from the
 *     glass (`sm:py-4`), and it is the same gap it keeps ABOVE itself: 16 over the
 *     card against 24 under it hung the whole list one step high in its own
 *     window. A phone is full bleed and pays none;
 *   - PEEK is what a fitted page leaves UNDER its last row, so the next
 *     project's band shows and the list never ends flush with the bottom of the screen.
 *
 * Touch keeps at least fifteen rows per project. A short landscape phone therefore
 * scrolls through useful history instead of paying a project header and pager every
 * three sessions. Pointer layouts retain the three-row emergency floor.
 */
const LIST_PEEK = 40;
const LIST_FOOT = 16;
/**
 * The lane the scrollbar travels in — and, being padding on the scroller, the air the
 * project sheets stand inside on a desk. The bar itself paints no track any more
 * (`index.css`), so without this the thumb would ride the sheets' own edge.
 */
const LIST_LANE = 12;
const LIST_GEOMETRY = {
  touch: { row: 49, chrome: 211 + LIST_PEEK, min: 15 },
  mouse: { row: 33, chrome: 149 + LIST_FOOT + LIST_PEEK, min: 3 },
  // A DESK SPENDS ITS CHROME SIDEWAYS. The machine strip that stands above the list
  // at every other size is the rail here, so those 40px go back to the rows and the
  // fleet footer under the list takes 29 of them again.
  desk: { row: 33, chrome: 138 + LIST_FOOT + LIST_LANE + LIST_PEEK, min: 3 },
} as const;

/**
 * How many sessions one project's page holds — the SCREEN's answer, not a
 * setting's.
 *
 * `vis.sessionsPerProject` (5/10/15) sized this page for a device it never
 * measured; the panel, the key and the hook that read it are gone. It is the
 * number the GATEWAY is asked for (`limit`, see `GatewayClient.listProjectPage`),
 * so rotating the device asks for the page the new screen holds instead of
 * recutting rows this app downloaded to hide most of.
 */
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
  /** The fleet-wide search, asked by the app bar above every machine chip. */
  query: string;
  onQuery: (next: string) => void;
  subscriptions: SessionSubscriptionHub | null;
  /** No machine is answering at all — the shell decides what to show instead. */
  onUnreachable?: (message: string | null) => void;
  onOpen: (conn: GatewayConn, sid: string, fresh?: boolean) => void | Promise<void>;
  /**
   * Whether this screen is the one on the glass. It stays MOUNTED behind an open
   * transcript — its rows, scope, scroll position and expanded projects are the
   * reader's own frame — and everything below reads this to keep fleet-wide work
   * off a screen nobody can see.
   */
  isVisible: boolean;
  /**
   * Open the fleet-wide search page — the same door the app bar's glass is. It is
   * `null` while that page is ALREADY the screen, which stands the list's pull-down
   * gesture (`lib/pull-to-search`) down: a hint promising a page the reader is
   * already standing on is the screen lying to them.
   */
  onSearch: (() => void) | null;
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

/**
 * The busy key for the fork that takes the WHOLE session: a turn's own id marks
 * a fork cut at that turn, and no turn id can collide with this word.
 */
const WHOLE_SESSION_FORK = 'whole-session';

export function SessionsScreen({
  conns,
  primary = null,
  query,
  onQuery,
  subscriptions,
  onUnreachable,
  onOpen,
  isVisible,
  onSearch,
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
  // Exactly one paired machine is always active. The saved primary owns the first
  // scope; if it changes while this mounted screen is behind Settings, it becomes
  // the scope on return. Pressing the selected tab cannot turn it off.
  const [scopePick, setScopePick] = useState<string | null>(() =>
    primaryKey ?? (conns[0] ? machineKey(conns[0]) : null),
  );
  useEffect(() => {
    if (primaryKey) setScopePick(primaryKey);
  }, [primaryKey]);
  const scope = resolveScope(machines, scopePick);
  // THE FIELD IS IMMEDIATE; THE SEARCH IS A GESTURE THAT ENDS. `query` is what the
  // reader is typing, `searchNeedle` is what typing RESTED on — and every answer, every
  // count and every filtered row on this screen belongs to the second one. The pause
  // used to live inside the network effect alone, so while it slept the screen still
  // re-filtered, re-ranked, threw away the transcript hits it had and re-drew the
  // header ON EVERY CHARACTER: the list jumped under the thumb a letter at a time and
  // the count flickered between an answer and nothing. Downstream of one settled
  // needle, a keystroke costs the field and nothing else, and what is on screen is
  // always a whole answer to a whole word.
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
  // Every machine's answer to ONE query, filed under the needle it answered — a
  // fleet search is several round trips that land at different times, and the
  // screen has to be able to say which of them are still out. `matches` (ranked
  // hits) and `rows` (sessions the hits named that this page had not loaded) come
  // back one after the other, so a machine files its matches first and its
  // hydrated rows a round trip later.
  const [searchAnswers, setSearchAnswers] = useState<SearchAnswers>(NO_SEARCH);
  // The create in flight and the project header that started it. Only that
  // header replaces its plus with the busy word.
  const [creating, setCreating] = useState<{ at: string | null; label: string } | null>(null);
  const [createError, setCreateError] = useState<string | null>(null);
  const [manageProjects, setManageProjects] = useState<{
    machine: FleetMachine;
    at: { top: number; left: number };
  } | null>(null);
  const forkAnchorEl = useRef<HTMLElement | null>(null);
  const pollStartedAt = useRef<number | null>(null);
  // Is the gateway pushing this list its fleet status, and when did the window last
  // cost a read? Refs, not state: the poll reads them on its own tick, and a cadence
  // must not re-run the effect that owns the timer.
  const fleetStreamingRef = useRef(false);
  const lastWindowReadAt = useRef(0);
  // The row action belongs to one session on one machine. Renaming needs an input
  // dialog; deleting asks through `ConfirmRow` exactly where that session row stood.
  // FORKING one session, from that row's own slide. The order is one value like
  // the start order above it: which row, on which machine, and where the panel
  // hangs — leaving it forgets all three.
  const [forkFlow, setForkFlow] = useState<{
    session: Session;
    conn: GatewayConn;
    at: { top: number; left: number };
  } | null>(null);
  // The turns that row can be cut at. `null` = still reading; the panel says so
  // rather than claiming the session has none.
  const [forkPoints, setForkPoints] = useState<{ rows: ForkPoint[] | null; error: string | null }>({
    rows: null,
    error: null,
  });
  // Which choice in the panel is running — the whole session, or one turn's id.
  const [forkBusy, setForkBusy] = useState<string | null>(null);
  // WHICH row the open panel is reading turns for, out of the render loop: the fleet
  // poll hands this screen a new machine object every few seconds and an effect that
  // depended on it would abort its own read on the frame it started.
  const forkSourceRef = useRef<{ sid: string; conn: GatewayConn } | null>(null);
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
  // Repaint when a read mark moves — opening a session clears its badge from here.
  const readMarks = useReadMarks();
  // Unsent words this device is holding, keyed by (gateway, session). An EMPTY
  // session that has some is DIRTY: it stays in the list — with a way back into
  // what you wrote, and a way to throw it away — instead of being hidden with
  // the words locked inside it.
  const draftMessages = useDraftMessages();

  // A session this device has never met is NOT unread: seed it at the turn count
  // it arrived with, so only answers that land AFTER this point raise a badge.
  // Without the seed, a fresh install would paint the whole fleet unread.
  useEffect(() => {
    const sessions = machines.flatMap((machine) => machine.sessions ?? []);
    void seedReadMarks(sessions);
  }, [machines]);

  // The app icon's badge is the SAME tally as the dots on these rows: one per
  // answer this device has not read. It is written from here because this is
  // the only place that sees every machine at once — and said again on wake,
  // because `VisNotify` moved the number while the app was away and told
  // nobody. `syncBadge` also drops the delivered alerts of sessions that have
  // since been read, which is what keeps the extension's count honest.
  useEffect(() => {
    void syncBadge(machines);
  }, [machines, readMarks]);
  useEffect(() => onWake(() => void reassertBadge()), []);

  // EVERY change to a machine's rows is anchored first.
  //
  // A cold fleet does not arrive at once and cannot: each gateway is its own round
  // trip. Every patch inserts rows ABOVE the sections below it — with two gateways
  // paired, the first machine's answer pushes the whole second machine down under a
  // reader who is looking at it, which is the list "jumping by itself" while its
  // projects load in. A failure does the same in reverse: a machine that stops
  // answering is dropped out of `All`, and its section leaves a hole.
  //
  // So the anchor belongs to the MUTATION, not to one caller: the top visible row
  // is measured here, and the layout effect below puts it back under the top edge.
  // It no-ops at the top of the list, so a first paint is unaffected.
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

  // The order the list is HELD in is renewed by reader actions that legitimately
  // move a row, and this ref is how a callback declared above the hook reaches it.
  const adoptRef = useRef<() => void>(() => {});

  // Ids this device just created, freshest first. `MINTED_KEEP` is past any burst
  // of taps and keeps the set from growing for the life of the screen.
  const [minted, setMinted] = useState<readonly string[]>([]);

  // ONE machine's list. Machines load independently on purpose: a gateway that is
  // asleep must not keep the machines next to it off the screen, and its failure
  // drops that machine out of the fleet view instead of taking the whole list down.
  //
  // It ANSWERS its failure as well as storing it: a retry the reader asked for has to
  // say what came back, in the tile that was pressed, and reading that off the state
  // this call is about to write would be reading it a paint too early.
  const loadMachine = useCallback(
    async (conn: GatewayConn, signal?: AbortSignal): Promise<string | null> => {
      const key = machineKey(conn);
      const api = clientFor(conn);
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
          const held = machinesRef.current.find(
            (machine) => machineKey(machine.conn) === key,
          );
          const merged = reconcileSessions(held?.sessions ?? null, rows);
          // The stable project totals arrive BESIDE the head window. Adopt both in one
          // patch so no intermediate frame tallies whichever session pages landed first.
          const reportedOverview = api.projectsOverview();
          const overview =
            reportedOverview && held?.overview && sameOverview(held.overview, reportedOverview)
              ? held.overview
              : reportedOverview;
          if (
            held &&
            held.error === null &&
            held.answered &&
            merged === held.sessions &&
            overview === held.overview
          )
            return;
          patchMachine(key, (machine) => ({
            ...machine,
            sessions: merged,
            overview,
            error: null,
            answered: true,
            isRemembered: false,
          }));
        };
        const next = await api.listSessions(signal);
        if (signal?.aborted) return null;
        alive();
        settle(next);
        return null;
      } catch (cause) {
        if (signal?.aborted) return null;
        const failure = (cause as Error).message;
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
      }
    },
    [patchMachine],
  );

  // The star belongs to the GATEWAY, and this is the one place that asks it to move.
  // The row is repainted first so the mark lands in the SAME commit as the tap, the
  // PATCH follows, and the row the gateway echoes replaces the guess. A star this
  // device could not deliver goes back: a screen must never keep saying something
  // the machine never heard.
  //
  // WHERE the row then belongs is not this device's arithmetic. A star bands its row
  // to the top of the list, and the list is the gateway's — so the mark is echoed
  // here and the ORDER is read back from whoever owns it, one read, right away
  // instead of at the next poll.
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
          // The order is taken only once the list that OWNS it has answered. Adopting
          // on the tap froze the order the star was tapped IN, and the promotion the
          // gateway then sent waited behind the pill (see `lib/order-epoch`).
          adoptRef.current();
        })
        .catch(() => patchMachine(key, withRank(before)));
    },
    [loadMachine, patchMachine],
  );

  // WHAT A RETRY IS DOING, on the tile that asked for it.
  //
  // A machine that is not answering is drained out of the switch and dropped from
  // `All`, and the one thing it can still do is come back — so its tile IS the retry
  // (see `MachineTab`). The press has to answer: `reconnecting...` while the probe is
  // in flight, `Unable to connect` in error ink when it came back dead, and nothing at
  // all before the first press, because a fleet's dead machines are quiet until they
  // are asked. A machine that answers loses its note along with its drained face.
  const [retries, setRetries] = useState<ReadonlyMap<string, 'busy' | 'failed'>>(
    () => new Map(),
  );
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

  // RECONNECTING A DARK MACHINE IS BACKGROUND WORK, and it is silent.
  //
  // It cannot ride the fleet load: a closed laptop TAKES the socket without refusing it,
  // so awaiting it made every poll as slow as the machine that is not there — and
  // `STALE_POLL_MS` then dropped the tick queued behind it, which is how one dead gateway
  // halved the refresh of every machine that was answering. It must not be painted
  // either: the reader is told about a machine coming BACK, never about this device
  // asking. So it runs beside the load, at most one probe per machine at a time, and the
  // only thing it can put on screen is an answer (see `loadMachine`).
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
      try {
        await Promise.all(
          paired.filter((conn) => !dark(conn)).map((conn) => loadMachine(conn, signal)),
        );
      } finally {
        if (background) pollStartedAt.current = null;
      }
    },
    [loadMachine, reconnectMachine],
  );

  // WORDS THIS DEVICE IS HOLDING ARE PART OF THE QUESTION IT ASKS.
  //
  // The overlay a list read carries (`dirty=`) is the one fact about the order
  // only this device knows, so the moment it changes the answer on screen is
  // stale — a session that was hidden at the bottom of its project now belongs
  // in the dirty band. The read is awaited first and the order adopted after it
  // lands: the reader WROTE those words, so the move is their own action, not
  // the surprise the pill exists for (see `lib/order-epoch`).
  const dirtyOverlay = useMemo(
    () =>
      Object.entries(draftMessages)
        .filter(([, message]) => draftMessageHasUnsent(message))
        .map(([key]) => key)
        .sort()
        .join('|'),
    [draftMessages],
  );
  const overlayRef = useRef(dirtyOverlay);
  useEffect(() => {
    if (overlayRef.current === dirtyOverlay) return;
    overlayRef.current = dirtyOverlay;
    const controller = new AbortController();
    void load(controller.signal).then(() => adoptRef.current());
    return () => controller.abort();
  }, [dirtyOverlay, load]);

  // A machine's NAME is not its transport, so renaming it must not refetch a thing —
  // but the banner reads that name off `machine.conn`, and `fleetKey` deliberately
  // ignores it, so an in-place rename saved to storage and then painted the old name
  // until the next pairing change. Re-hydrating keeps every row (`reconcileMachines`
  // hands the surviving machine its new connection) and reloads nothing.
  //
  // The same holds for the FACTS this screen never paints but the rest of the app reads
  // off `machine.conn`: the machine `id` backfilled after pairing and the `alts` learned
  // by address recovery. A row hands its OWN conn to `onOpen`, which becomes the active
  // connection and later the machine Settings opens on — so a fleet still holding the
  // pre-backfill conn opened Settings on an id-less machine and its panels never came.
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

  // THE LIST BEHIND AN OPEN TRANSCRIPT IS ALREADY LOADED, or leaving that transcript is
  // a flicker.
  //
  // Coming back to a killed app is normally coming back INTO a session: the OS drops a
  // backgrounded webview, the restored hash mounts the transcript, and this screen is
  // parked behind it holding nothing but its skeleton. Gating the fleet's FIRST read on
  // being visible then began that read on the very frame the reader pressed Back on —
  // measured against a gateway on localhost, eight frames of skeleton before the rows
  // replaced it, and a phone reaching a laptop over Wi-Fi pays the whole round trip in
  // that same place. So the read that gives this list its rows AT ALL happens wherever
  // the reader is. It is ONE read: it stands down the moment there is something to paint
  // (a cached fleet already is), and the five-second poll below stays the business of the
  // screen that is on the glass.
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
      if (fleetStreamingRef.current && now - lastWindowReadAt.current < STREAMED_POLL_MS)
        return;
      lastWindowReadAt.current = now;
      void load(controller.signal, true);
    };

    void load(controller.signal);
    lastWindowReadAt.current = Date.now();
    // The session-list head is already the reachability check and carries live/idle
    // totals, so do not add a second health request. Five seconds bounds how long a
    // machine can still look active after it stops answering. Cheap on BOTH ends — an
    // unchanged fleet comes back as a 304 with no body (see `GatewayClient.listSessions`),
    // and `load(_, true)` drops a tick that fires while the previous one is still in
    // flight instead of queueing it. A frozen webview runs no timers. Do not trust
    // `document.visibilityState` here: a resumed Capacitor webview can keep reporting
    // `hidden` while this screen is on the glass.
    const timer = window.setInterval(refreshLiveStates, 5_000);
    // Waking is the one moment the rows are guaranteed stale, and a suspended
    // poll may still be latched: drop the latch, then refresh.
    const stopWake = onWake(({ awayMs }) => {
      pollStartedAt.current = null;
      // Away long enough that "where you were" stopped being a place: come back to
      // what is current, the ORDER included (see `lib/order-epoch`).
      if (awayMs >= EPOCH_STALE_AWAY_MS) adoptRef.current();
      refreshLiveStates();
    });
    return () => {
      controller.abort();
      window.clearInterval(timer);
      stopWake();
    };
    // A connection identity change should preserve the existing frame until its data arrives.
  }, [fleetKey, isVisible, load]);

  // A fleet frame normally carries the whole answer for one row, so live and title
  // changes repaint without a window read. A SETTLED frame is the exception: metadata
  // can raise NEW before the finished transcript page exists in this device's cache. It
  // answers false below so the canonical list read warms that page before painting the
  // finished row. False also covers membership news for a row this window does not hold.
  const applyFleetFrame = useCallback(
    (event: SseEvent): boolean => {
      const sid =
        typeof event.session_id === 'string'
          ? event.session_id
          : typeof event.sid === 'string'
            ? event.sid
            : '';
      if (!sid) return false;
      let update: Partial<Session>;
      if (event.type === 'session.status') {
        if (typeof event.is_live !== 'boolean') return false;
        if (!event.is_live) return false;
        update = {
          live: event.is_live,
          is_awaiting_input: event.is_awaiting_input === true,
          current_turn_id:
            typeof event.current_turn_id === 'string' ? event.current_turn_id : null,
        };
      } else if (typeof event.title === 'string' && event.title.length > 0) {
        update = { title: event.title };
      } else {
        return false;
      }
      const holders = machinesRef.current.filter((machine) =>
        machine.sessions?.some((row) => row.id === sid),
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
      return holders.length > 0;
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
      refreshTimer = window.setTimeout(() => void load(undefined, true), 120);
    };
    const stopState = subscriptions.subscribeFleetState((streaming) => {
      fleetStreamingRef.current = streaming;
    });
    const unsubscribe = subscriptions.subscribeFleet((event) => {
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
  }, [applyFleetFrame, isVisible, load, subscriptions]);

  useLayoutEffect(() => {
    const anchor = refreshAnchorRef.current;
    const viewport = listRef.current;
    refreshAnchorRef.current = null;
    if (!anchor || !viewport || viewport.scrollTop <= 2) return;
    const offset = rowOffset(viewport, anchor.id);
    if (offset !== null) viewport.scrollTop += offset - anchor.offset;
  }, [machines]);

  // Coming back to the list means coming back to WHERE you were in it. Opening a
  // session unmounts this screen, so the mark outlives it in a module (see
  // `lib/list-scroll`) and is put back on the first paint that has the rows to
  // put it back on: a cached fleet repaints instantly, a cold one lands a beat
  // later, and either way the row that was under the top edge goes back under it
  // even though the session just visited has jumped to the top of the list.
  useLayoutEffect(() => {
    if (restoredRef.current) return;
    // Off the glass there is no layout to restore INTO, and a mark spent where the
    // reader cannot see where it landed is their place lost: the warm-up above fills
    // this list behind an open session, so the fleet can now finish loading while it is
    // hidden — and finishing is what gives up on a mark that does not fit yet.
    if (!isVisible) return;
    const mark = parkedListScroll();
    if (!mark) {
      restoredRef.current = true;
      return;
    }
    const viewport = listRef.current;
    if (!viewport) return;
    // Once every machine in scope has answered, this IS the list: a mark that
    // still does not fit points at rows that are gone, and retrying it on every
    // later paint would fight the reader instead of serving them.
    if (applyListScroll(viewport, mark, (id) => rowOffset(viewport, id)) || isFleetLoaded(machines, scope)) {
      restoredRef.current = true;
      forgetListScroll();
    }
  });

  // The reader scrolling is the reader deciding: stop trying to restore.
  useListScrollPark(listRef, () => {
    restoredRef.current = true;
  });

  // AT THE TOP OF THE LIST, A PULL IS A QUESTION ABOUT SEARCH. The glass that opens
  // the search page sits in the far top corner of the app bar; the thumb already
  // reading this list has a gesture for it, and every native list answers it.
  usePullToSearch(listRef, hintRef, setPullPhase, onSearch);

  // Transcript + title search runs server-side and RANKED (see `rank` below), and
  // it is a fleet ROUND TRIP per machine over a whole transcript store — hundreds
  // of milliseconds, not a keystroke. This effect only asks, and it is handed the
  // needle typing already RESTED on (`searchNeedle`), so it asks once per pause. A
  // superseded query is cancelled twice over: the pause upstream never files a needle
  // nobody stopped on, and a request already in flight is aborted here.
  // What decides what is on SCREEN is neither: an answer is filed under the needle
  // it was asked for, so a late reply to an abandoned query is ignored by
  // construction rather than by luck.
  //
  // Every machine paints the MOMENT IT ANSWERS, and its matches paint one round
  // trip before its hydrated rows. Waiting on the whole fleet made every search as
  // slow as its slowest machine, and holding the hits back until up to
  // `SEARCH_HYDRATE_MAX` per-session fetches had all landed spent another round
  // trip hiding matches the gateway had already found — a long silent wait, which
  // is exactly what the search was reported for.
  useEffect(() => {
    const needle = searchNeedle;
    if (!needle) return;
    // WHICH MACHINES ARE EVEN ASKED. A gateway that is not answering is not asked a
    // question: neither the one whose list read failed nor the one that already ate a
    // whole search deadline in silence. Both still count as ASKED and answer at once as
    // unreached — dropping them silently would make the search look complete when part
    // of the fleet was never read (see `searchFanout`).
    const fanout = searchFanout(
      connsRef.current,
      machinesRef.current,
      scope,
      searchSilentRef.current,
    );
    setSearchAnswers({
      needle,
      asked: fanout.asked,
      byMachine: new Map(fanout.dark.map((key) => [key, UNREACHED])),
    });
    const reachable = fanout.ask;
    if (reachable.length === 0) return;
    const controller = new AbortController();
    const answer = (key: string, entry: SearchAnswer) =>
      setSearchAnswers((prev) =>
        prev.needle === needle
          ? { ...prev, byMachine: new Map(prev.byMachine).set(key, entry) }
          : prev,
      );
    for (const conn of reachable) {
      const key = machineKey(conn);
      const api = clientFor(conn);
      void (async () => {
        // The search's own deadline (`SEARCH_REACH_MS`), not the transport's: a
        // blackholed socket ends only when someone cancels it, and the whole fleet's
        // progress was hostage to the machine that never spoke.
        const reach = new AbortController();
        const giveUp = () => reach.abort();
        controller.signal.addEventListener('abort', giveUp, { once: true });
        const expiry = window.setTimeout(giveUp, SEARCH_REACH_MS);
        // One machine failing to search (asleep, refused, older gateway, out of time)
        // must not blank the matches the others found — and must not be filed as an
        // answer it never gave, which is how a dead gateway used to report "no
        // matches on this machine".
        const found = await api.searchSessionMatches(needle, reach.signal).catch(() => null);
        // The deadline is spent; the read is the reader's again. The effect's own
        // cancellation stays wired to this reach for as long as the effect lives, so a
        // query the user has replaced still aborts the flight it started.
        window.clearTimeout(expiry);
        if (controller.signal.aborted) return;
        if (found === null) {
          // NOW KNOWN DARK. The next query skips this machine outright instead of
          // spending another `SEARCH_REACH_MS` rediscovering the same silence; its
          // next answered list read takes the mark off again (see `loadMachine`).
          searchSilentRef.current.add(key);
          answer(key, UNREACHED);
          return;
        }
        answer(key, { matches: found, rows: [], reached: true });
        // The list is PAGED. Intersecting the hits with the rows already loaded
        // meant search could only find what was on screen; a hit in a session
        // further down the fleet's ordering vanished. Fetch those rows by id —
        // AFTER the matches above are already on screen.
        const loaded = new Set(
          (machinesRef.current.find((machine) => machineKey(machine.conn) === key)
            ?.sessions ?? []).map((session) => session.id),
        );
        const missing = found
          .filter((match) => !loaded.has(match.sessionId))
          .slice(0, SEARCH_HYDRATE_MAX);
        if (missing.length === 0) return;
        const rows = await Promise.all(
          missing.map((match) =>
            api.session(match.sessionId, controller.signal).catch(() => null),
          ),
        );
        if (controller.signal.aborted) return;
        answer(key, {
          matches: found,
          rows: rows.filter((row): row is Session => row !== null),
          reached: true,
        });
      })();
    }
    return () => {
      controller.abort();
    };
  }, [searchNeedle, fleetKey, scope]);

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
  const live = searchAnswers.needle === searchNeedle ? searchAnswers : null;
  const searchAsked = live?.asked ?? NO_MACHINES;
  const searchAnswered = useMemo(
    () => new Set(searchAsked.filter((key) => live?.byMachine.has(key) === true)),
    [live, searchAsked],
  );
  // STILL ASKING — the field is ahead of the needle the list answers (inside the pause),
  // no needle has been filed yet, or a machine that was asked has yet to come back. This
  // is the one fact the screen owed the reader and did not have.
  const searchPending =
    searching &&
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
  // WHERE the gateway put each match: its index in that machine's answer, which
  // is the gateway's own order — running sessions first, then freshest first.
  // The place, not the relevance band, is what the list sorts by; a session's
  // index is only ever compared with others from the SAME machine, since a
  // machine's rows are filtered and ordered inside its own section.
  const searchPlaces = useMemo(() => {
    if (!live) return null;
    const places = new Map<string, number>();
    for (const entry of live.byMachine.values())
      entry.matches.forEach((match, index) => places.set(match.sessionId, index));
    return places;
  }, [live]);
  // Sessions a transcript hit named that this machine had not paged in yet, per
  // machine key. Kept beside the list instead of merged into it: the 10s poll
  // rewrites `machine.sessions` from the gateway's own paged answer.
  const searchHits = useMemo(() => {
    const byMachine = new Map<string, Session[]>();
    if (live) for (const [key, entry] of live.byMachine) byMachine.set(key, entry.rows);
    return byMachine;
  }, [live]);

  const inScope = useMemo(() => scopedMachines(machines, scope), [machines, scope]);

  // The list is only "still loading" while NOTHING has answered: one slow machine
  // must not hold the machines beside it off the screen.
  const sessions = useMemo(() => {
    if (machines.length === 0) return [];
    const rows = inScope.flatMap((machine) => machine.sessions ?? []);
    return inScope.some((machine) => machine.sessions !== null) || isFleetLoaded(machines, scope)
      ? rows
      : null;
  }, [inScope, machines, scope]);

  // Filtering happens INSIDE each machine: two checkouts of the same repo on two
  // machines are two projects, and a folder name never merges them.
  const filtered = useMemo(() => {
    const needle = searchNeedle.toLowerCase();
    return inScope.map((machine) => {
      const base = clientFor(machine.conn).base;
      const draftFor = (session: Session) => draftMessages[draftMessageKey(base, session.id)];
      // Server-side transcript hits this machine had not paged in are part of the
      // list a query filters: without them search only finds what is on screen.
      const hits = needle ? (searchHits.get(machineKey(machine.conn)) ?? []) : [];
      const titleHit = (session: Session) =>
        needle.length > 0 && (session.title ?? '').toLowerCase().includes(needle);
      // A dirty row has no title and no transcript: what waits in its composer —
      // the words AND the names of the files staged with them — is the only thing
      // a query could match it on.
      const metaHit = (session: Session) =>
        needle.length > 0 &&
        (sessionSearchText(session).includes(needle) ||
          draftSearchText(draftFor(session)).includes(needle));
      // WHICH ROWS THESE ARE IS THE GATEWAY'S ANSWER (`GatewayClient.listSessions`):
      // it drops the abandoned taps itself and keeps the ones this device told it are
      // holding unsent words. A query NARROWS that list; it never re-decides it.
      const sessions = withSearchHits(machine.sessions ?? [], hits).filter(
        (session) =>
          !needle || titleHit(session) || metaHit(session) || matches?.has(session.id) === true,
      );
      // A query RE-ORDERS what it matched and the GATEWAY decides how: the
      // search answer arrives running-sessions-first, then FRESHEST first — the
      // very order the gateway lists sessions in — so a query narrows the list
      // instead of reshuffling it, and the dates down the rows only ever fall.
      // Painting the band instead (`SessionMatch.rank`: title, then the user's
      // words, then the assistant's) buried this morning's session under every
      // year-old title holding the word. Rows the gateway did not place — an
      // unsent draft in this device's composer, local metadata — fall in behind.
      // A SEARCH is the one answer this device still bands for itself: it is a
      // COMPLETE match set, so lifting starred and unsent rows inside it can move
      // nothing out of a page. The unqueried list is the gateway's own, banded there,
      // and is handed on exactly as it arrived.
      if (!needle) return { machine, sessions };
      return {
        machine,
        sessions: sessionOrder(
          searchOrder(sessions, (session) => searchPlaces?.get(session.id) ?? SEARCH_UNPLACED),
          {
            favoriteRank,
            hasDraftMessage: (session) => draftMessageHasUnsent(draftFor(session)),
          },
        ),
      };
    });
  }, [inScope, searchNeedle, matches, searchPlaces, searchHits, draftMessages]);

  // NOTHING MOVES WHILE THE READER IS LOOKING AT IT (see `lib/order-epoch`).
  //
  // The gateway's key is content time only now, and no band lifts a running or
  // parked session over the rest, so nothing this device does can move a row.
  // One mover is left and it cannot be removed: another machine — or another
  // turn on this one — writing content while this list is on screen. The answer
  // that arrives is correct and the rows still slide under the thumb, which was
  // the whole report. So the order is HELD as of the last moment the reader
  // agreed to it: polls repaint those rows IN PLACE, arrivals deeper than
  // everything held append (that is paging), and a promotion waits behind the
  // count above the list.
  //
  // The view key is the QUESTION the rows answer: scoping to another machine or
  // typing a query is a different answer, with no reading position to protect.
  //
  // A FLEET STILL ANSWERING IS NOT AN ORDER: machines land one by one and a query
  // is served by a round trip per gateway, so an epoch taken mid-arrival would
  // park the machine that answered second behind the pill. Until every machine in
  // scope has spoken — and while a query is live at all, because a search is the
  // reader's own question and the gateway's answer to it IS the order — the rows
  // are painted exactly as they came.
  const naturalIds = useMemo(
    () => filtered.flatMap((entry) => entry.sessions.map((session) => session.id)),
    [filtered],
  );
  const isOrderSettled =
    !searchNeedle && !searchPending && sessions !== null && isFleetLoaded(machines, scope);
  const { epoch, adopt } = useOrderEpoch(
    `${scope}\u0000${searchNeedle}`,
    naturalIds,
    isOrderSettled,
  );
  adoptRef.current = adopt;

  // Regression, user report (paraphrased: a session I just started should not
  // need a tap to appear): the pill is for rows ANOTHER machine wrote under a
  // still thumb. A session this device just created is the reader's own action,
  // so it is admitted into the held order at once. A few are kept, because the
  // ones before the last are not necessarily in the epoch yet either.
  const mintedSet = useMemo(() => new Set(minted), [minted]);

  const heldRows = useMemo(
    () =>
      filtered.map((entry) => {
        // A row this device is HOLDING WORDS for is the reader's own action too.
        // The gateway lifted it into the dirty band BECAUSE this device said so
        // (`dirty=`), so the move is already agreed to: making the writer tap a
        // pill to see where their own unsent sentence went is the same complaint
        // that admitted a just-created session.
        const admitted = new Set(mintedSet);
        for (const id of dirtySessionIds(clientFor(entry.machine.conn).base))
          admitted.add(id);
        return {
          machine: entry.machine,
          admitted,
          ...holdOrder(
            epoch,
            entry.sessions,
            (session) => ({
              id: session.id,
              millis: sessionMillis(session),
            }),
            admitted,
          ),
        };
      }),
    [epoch, filtered, mintedSet, draftMessages],
  );

  const pendingCount = useMemo(
    () => heldRows.reduce((count, entry) => count + entry.pending.length, 0),
    [heldRows],
  );
  // A filter is a FLEET question: it runs on every machine in scope, so the header
  // reports what came back and from how many of them.
  const searchCounts = useMemo(() => searchTally(filtered), [filtered]);

  const visible = useMemo(
    () => (sessions === null ? null : heldRows.flatMap((entry) => entry.rows)),
    [heldRows, sessions],
  );

  // Per-machine tallies for the strip and the machine headers.
  const tallies = useMemo(
    () =>
      new Map(
        machines.map((machine) => [
          machineKey(machine.conn),
          machineCounts(machine, sessionIsLive, (session) => unreadTurnCount(session) > 0),
        ]),
      ),
    [machines, readMarks],
  );
  const scopeMachine = scope
    ? (machines.find((machine) => machineKey(machine.conn) === scope) ?? null)
    : null;

  // One hue per paired machine, assigned from the machine's own key, so a rail
  // keeps its colour across reloads and reorderings and two machines side by side
  // never share one. Colour is what the eye reads before the name, and the same
  // hue rides the scope chip above the list and the rail down its left.
  const machineColors = useMemo(
    () => assignMachineColors(machines.map((machine) => machineKey(machine.conn))),
    [machines],
  );
  // Pairing order still owns sections, hues and every persisted identity. The switch is
  // only a destination list: the answering primary leads, every other answering machine
  // keeps pairing order, and destinations that no longer answer follow all of them.
  const switcherMachines = useMemo(() => {
    const answering = machines.filter((machine) => !machine.error);
    const primaryIndex = primaryKey
      ? answering.findIndex((machine) => machineKey(machine.conn) === primaryKey)
      : -1;
    if (primaryIndex > 0) answering.unshift(...answering.splice(primaryIndex, 1));
    return [...answering, ...machines.filter((machine) => Boolean(machine.error))];
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


  const createSession = useCallback(async (on: GatewayConn, root: string) => {
    setCreating({
      at: `${clientFor(on).base}\u0000${root}`,
      label: 'Creating...',
    });
    setCreateError(null);
    try {
      const session = await clientFor(on).createSession({ root });
      // Open before refreshing the fleet. The full list walk is background work,
      // while the session the reader just requested is their immediate destination.
      if (session.id) setMinted((was) => [session.id, ...was].slice(0, MINTED_KEEP));
      if (session.id) await onOpen(on, session.id, true);
      void load();
    } catch (cause) {
      setCreateError((cause as Error).message);
    } finally {
      setCreating(null);
    }
  }, [load, onOpen]);


  /** Open the conversation-fork question under the row action that invoked it. */
  const startFork = useCallback((session: Session, conn: GatewayConn, anchor: HTMLElement) => {
    forkAnchorEl.current = anchor;
    const at = menuPosition(anchor.getBoundingClientRect(), MENU_WIDTH);
    if (!at) return;
    forkSourceRef.current = { sid: session.id, conn };
    setForkPoints({ rows: null, error: null });
    setForkBusy(null);
    setForkFlow({ session, conn, at });
  }, []);

  const leaveFork = useCallback((restoreFocus = false) => {
    setForkFlow(null);
    setForkBusy(null);
    if (restoreFocus) forkAnchorEl.current?.focus();
  }, []);

  // Read turns once per opening, keyed by row rather than by objects replaced by fleet polling.
  const forkSid = forkFlow?.session.id ?? null;
  const forkMachine = forkFlow ? machineKey(forkFlow.conn) : null;
  useEffect(() => {
    const source = forkSourceRef.current;
    if (!forkSid || !forkMachine || !source) return;
    const controller = new AbortController();
    void clientFor(source.conn)
      .forkPoints(source.sid, controller.signal)
      .then((rows) => setForkPoints({ rows, error: null }))
      .catch((cause) => {
        if (controller.signal.aborted) return;
        setForkPoints({ rows: [], error: (cause as Error).message });
      });
    return () => controller.abort();
  }, [forkSid, forkMachine]);

  /**
   * Cut the fork. `throughTurnId` is the LAST turn it keeps; without one the
   * fork carries the whole conversation. The fork is a session of its own, so
   * the app goes straight into it — the source row is untouched behind it.
   */
  const runFork = useCallback(
    async (throughTurnId?: string) => {
      if (!forkFlow) return;
      const { session, conn } = forkFlow;
      setForkBusy(throughTurnId ?? WHOLE_SESSION_FORK);
      setForkPoints((points) => ({ ...points, error: null }));
      try {
        const forked = await clientFor(conn).forkSession(session.id, throughTurnId);
        setForkFlow(null);
        setForkBusy(null);
        await onOpen(conn, forked.id);
      } catch (cause) {
        setForkBusy(null);
        setForkPoints((points) => ({ ...points, error: (cause as Error).message }));
      }
    },
    [forkFlow, onOpen],
  );

  // Apply a successful gateway deletion to exactly the machine that owned it. The
  // gateway already answered which ids disappeared, so neither a session nor a project
  // removal re-downloads the fleet merely to rediscover that answer.
  const forgetSessions = useCallback(
    (conn: GatewayConn, ids: string[], project?: ManagedProject) => {
      const api = clientFor(conn);
      for (const sid of ids) clearDraftMessage(draftMessageKey(api.base, sid));
      if (ids.length > 0) void flushDraftMessages();

      const gone = new Set(ids);
      patchMachine(machineKey(conn), (machine) => {
        const rows = machine.sessions;
        const sessions = rows && gone.size > 0
          ? rows.filter((row) => !gone.has(row.id))
          : rows;
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
            };
          }
        }
        if (sessions === rows && overview === machine.overview) return machine;
        return { ...machine, sessions, overview };
      });
    },
    [patchMachine],
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
      if (failed > 0)
        throw new Error(`${failed} of ${ids.length} sessions could not be deleted.`);
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
      rename: renameSession,
      fork: startFork,
      requestDelete: startDelete,
      toggleStar,
    }),
    [onOpen, renameSession, startFork, startDelete, toggleStar],
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
    [
      rowCommands,
      deleting,
      actionBusy,
      actionError,
      confirmDelete,
      cancelDelete,
    ],
  );
  const rowContext = useMemo<SessionRowsContext>(
    () => ({
      getClient: clientFor,
      drafts: draftMessages,
      matches,
      needle: searchNeedle,
      actions: rowActions,
    }),
    [draftMessages, matches, searchNeedle, rowActions],
  );
  const projectCreation = useMemo<ProjectCreation>(
    () => ({ state: creating, start: createSession }),
    [creating, createSession],
  );

  const pageSize = useSessionsPerPage();

  // Machine → project → sessions. The machine is the organizer, so its sections
  // are built from ITS rows only.
  // Machine → project → sessions. The machine is the organizer, so its sections are
  // built from ITS projects only.
  //
  // Unfiltered, those projects are the GATEWAY's (`/v1/projects/overview`): every
  // project it holds, with its own counts, whether or not a row of it is in the window
  // this device read. They used to be a grouping of downloaded rows, so a project only
  // existed once its rows had drained in and its header counted the window. A QUERY is
  // the one answer this device holds complete, and its groups are the rows that
  // matched, grouped here.
  const sections = useMemo(
    () =>
      heldRows.map((entry) => ({
        machine: entry.machine,
        // Carried beside the machine because every project group holds a page of its
        // own now, under the same agreement this screen made (`lib/order-epoch`).
        reading: {
          pageSize,
          epoch,
          admitted: entry.admitted,
          isVisible,
        },
        // Group identity and every create action keep the gateway's canonical path.
        // Home-shortening is paint only; feeding `~/vis` back as an API root is how an
        // older gateway produced the impossible `/…/vis/~/vis` directory.
        groups: searching
          ? searchGroups(entry.rows, (session) => unreadTurnCount(session) > 0)
          : projectGroups(
              entry.machine.overview,
              entry.rows,
              (session) => unreadTurnCount(session) > 0,
            ),
      })),
    [heldRows, searching, readMarks, pageSize, epoch, isVisible],
  );

  // The projects the "remove sessions" step offers are the ones this machine HAS, as
  // its gateway counted them — the same rows and the same numbers as the headers in
  // the list, so a row that promises to remove 975 transcripts under a header reading
  // 712 is impossible by construction.
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

  // A dead gateway is not a sessions problem: there is nothing to navigate, so the
  // shell drops us on the Machines screen instead of rendering a session list
  // shaped like an error. Reporting it is this screen's only job here.
  //
  // Only TRANSITIONS are reported. A fresh mount starts with `loadError === null`,
  // and announcing "reachable" before the first request has answered is how this
  // screen used to un-gate the shell, get itself re-mounted, fail again, and gate
  // the shell again — a mount/fail/unmount loop that hammered the dead gateway
  // with thousands of requests per second instead of resting on Machines.
  //
  // With several machines paired only a TOTAL blackout is an error: one machine
  // asleep is a degraded section inside a list that still works.
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

  const isDesk = useDeskRail();

  // A rail row is an INDEX ENTRY: it takes the list to a band and changes nothing
  // else — not the scope, not the page, not the fold. `scrollIntoView` would walk
  // every scrollable ancestor to get there and can take the whole page with it; the
  // list has exactly one scroller, and this moves that one.
  const jumpToProject = useCallback((machine: string, root: string) => {
    const list = listRef.current;
    const band = Array.from(
      list?.querySelectorAll<HTMLElement>('[data-project-root]') ?? [],
    ).find((el) => el.dataset.machine === machine && el.dataset.projectRoot === root);
    if (!list || !band) return;
    list.scrollTop += band.getBoundingClientRect().top - list.getBoundingClientRect().top;
  }, []);

  // A RAIL IS FOR A FLEET, AND ONE MACHINE IS NOT ONE. Measured at 1280x800 against a
  // gateway with a single machine paired: 236px — 18% of the window — spent on one
  // machine row and three project rows whose own bands stand on the list two pixels
  // to the right of them. It earns that column the moment there is a second machine
  // to choose between; until then the projects verb it hosts stands in the footer.
  // A MACHINE'S HUE IS A COMPARISON TOO, and after two reports (paraphrased: no left
  // rail on the phone either — and then, with three machines on screen: bin that rail
  // on the left) nothing in the list wears it at all: a machine's block is separated
  // by the trough it opens on and named by its own landmark.
  const isFleet = machines.length > 1;
  const showRail = isDesk && isFleet;
  // THE RAIL IS BUILT FROM WHAT THE LIST IS ALREADY HOLDING, and only on a desk,
  // because only a desk paints it: one row per machine in the switcher's own order,
  // one per project band on screen.
  const railMachines = useMemo(
    () =>
      showRail
        ? switcherMachines.map((machine) => {
            const key = machineKey(machine.conn);
            const isDown = Boolean(machine.error);
            return {
              key,
              name: machineLabel(machine.conn),
              count: tallies.get(key)?.sessions ?? 0,
              mark: <MachineMark color={machineColor(machineColors, key)} isHollow={isDown} />,
              isActive: scope === key,
              onPress: () => (isDown ? void retryMachine(machine.conn) : selectScope(key)),
            };
          })
        : [],
    [showRail, switcherMachines, tallies, machineColors, scope, selectScope, retryMachine],
  );

  // The hue only rides a project row when there is more than one machine in view:
  // with a single machine every row would wear the same block, which says nothing.
  const railProjects = useMemo(
    () =>
      showRail
        ? sections.flatMap(({ machine, groups }) => {
            const key = machineKey(machine.conn);
            return groups.map((group) => ({
              key: `${key}\u0000${group.root}`,
              name: group.label,
              count: group.tally.count,
              mark:
                sections.length > 1 ? (
                  <MachineMark color={machineColor(machineColors, key)} />
                ) : null,
              onPress: () => jumpToProject(key, group.root),
            }));
          })
        : [],
    [showRail, sections, machineColors, jumpToProject],
  );

  // ON A DESK THE FLEET IS THE RAIL, so the row above the list is left with the
  // page's NOTICES: a share to place, how far a search reached, a create with no
  // button to speak from. With none of them it does not paint at all — an empty band
  // is 40px of paper the list could have filled with rows.
  const showStrip =
    machines.length > 0 &&
    (!isDesk || Boolean(share) || (searching && sessions !== null) || creating !== null);

  // What the rail cannot say, in the footer the desk has room for: both counts are
  // the GATEWAY's own (`machineCounts`), never a count of the rows this device holds.
  const fleetSessions = machines.reduce(
    (total, machine) => total + (tallies.get(machineKey(machine.conn))?.sessions ?? 0),
    0,
  );

  // WITH ONE MACHINE THERE IS NOTHING TO CHOOSE, so the footer NAMES it instead of
  // counting it: the rail's single row said `visgw`, and a count of one machine says
  // less than the name of the one whose rows are on screen.
  const soleMachine = machines.length === 1 ? machines[0] : null;
  // ONE VERB, AND TWO PLACES IT CAN STAND: the strip on a phone, the rail's own
  // PROJECTS caption on a desk. It is built here so neither paint can grow a second
  // folder with different words behind it (`ui.test.tsx` counts the call site).
  const projectsVerb =
    scopeMachine && !scopeMachine.error ? (
      <MachineProjectsButton
        isQuiet={isDesk && !showRail}
        machine={machineLabel(scopeMachine.conn)}
        onPress={(anchor) => openManageProjects(scopeMachine, anchor)}
      />
    ) : null;
  if (loadError) return null;

  return (
    <section aria-label="Sessions" className="mx-auto flex h-full min-h-0 w-full max-w-[1400px] flex-col pb-[env(safe-area-inset-bottom)] pl-[env(safe-area-inset-left)] pr-[env(safe-area-inset-right)] pt-0 transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none sm:px-6 sm:py-4">
      {/* On phones this panel sits FLUSH under the app header, whose own `border-b`
          already draws the rule below the Vis mark. A `border-y` here stacked a
          second hairline on top of it, so the Sessions tab wore a 2px seam while
          Machines (which floats its cards below a gap) wore 1px. Bottom edge only;
          the full box comes back once the panel detaches at `sm`. */}
      {/* THE CARD OWNS ITS TOP EDGE ON A PHONE, AND NOTHING ELSE. Its left side used to
          be a frame: a neutral 2px rule under the chrome bands, the machine's own hue
          down everything that machine owned — because a rail beside a border is two lines
          doing one job, and a rail that is a BORDER also steals 2px of layout the trailing
          edge has no match for. Reported (paraphrased: no left rail on the phone either,
          there is only ever one machine): with a fleet of one that frame ran the height of
          the glass to say which of one. The hue is a comparison and paints only above a
          fleet now, so under it the phone card is paper with rows on it, and the rows keep
          the 2px the frame used to take. From `sm` up there is no card at all — the
          projects are sheets standing on the page. */}
      {/* THE SWITCHER STANDS OUTSIDE WHAT IT SWITCHES, AND IT IS ONE OBJECT.
          The chips used to sit inside the machine card's own header, so the control
          that picks a machine looked like part of that machine's own answer. They are
          a segmented switch on the page's paper now: one track, the chosen machine a
          raised tile inside it. Exactly one machine is always chosen; selecting another
          moves the scope, while pressing the chosen machine leaves it active.

          THE SWITCH IS ALWAYS THERE, EVEN FOR A FLEET OF ONE. The tile is not only a
          choice, it is the label of everything under it: which computer these projects
          and sessions are on, in the machine's own hue, in the same place whatever the
          fleet size. One machine is one tab, already pressed, and the second lands beside it.

          THIS ROW HOLDS THE SWITCH AND THE MACHINE'S ONE VERB, AND NOTHING ELSE.
          A second band used to stand inside the card under it: the machine's name
          again, "2 projects - 1080 sessions", and these same controls. It named a
          machine the chips had just named, counted what every project header below it
          already counts, and spent a whole row of a phone's glass doing it. The band
          is gone, so the card starts at the first project header.

          NO OVERFLOW CONTROL STANDS HERE. A `⋯` beside the switch held two rows —
          `Manage projects`, which is the sheet the machine's own folder mark already
          opens, and
          `Machine settings`, which the Machines tab and the app bar's own cog both
          open — so it was a menu whose every answer was one tap away without it.
          The row is a switch and one mark, and both say what they do.

          ONE INSET PER EDGE, AND BOTH ENDS STAND ON THE PAPER. Standing on the page's
          paper means wearing the PAGE's side edges, and the section above already
          spells them (`sm:px-6`). `px-3` is here for the phone alone, where that
          section is full bleed and the ink edge is the app bar's own 12px; `sm:pr-4`
          keeps the same kind of inset once the card detaches. Nothing reclaims the
          gutter any more — the `-mr-3 sm:-mr-4` edge treatment left with the `⋯` that
          needed it, so the switch's left edge and the verb's right edge are the same
          distance from the paper: 12 and 12 of a 390 phone (strip at 12, the projects
          mark ending at 378), and 984 inside a card that ends at 1000 on a 1024 desk. */}
      {/* ONE TOP INSET, NOT TWO. The section already spells the page's top edge
          (`sm:pt-4`); this row spelled a second one (`sm:pt-8`) on top of it, so on
          a desk the machine strip started 56px under the app bar and the card 97px
          under it — a hand's width of paper above a list that then ran out of
          screen before it ran out of rows. The phone keeps its own `pt-6`: there
          the section is full bleed and this row IS the first thing under the bar. */}
      {showStrip && (
        <div className="relative z-10 flex flex-wrap items-center gap-x-1.5 gap-y-2 px-3 pb-3 pt-6 sm:flex-nowrap sm:pb-3 sm:pl-0 sm:pr-4 sm:pt-0">
          {/* The switch owns the leading space of this row: it GROWS, so the machine's
              verb stands at the trailing inset without an auto margin that would fight
              the search report for the same free space. The track inside it keeps its
              own compact width and scrolls a fleet that outgrows the row. */}
          {/* The switch IS the phone's fleet. On a desk those machines stand in the
              rail instead, and this row would be a second place to press for the same
              one thing. */}
          {!isDesk && (
          <div role="group" aria-label="Machines" className="flex min-w-0 flex-1">
            <MachineSwitcher>
                {/* The machine tabs are the groups, and exactly one is always active. */}
              {switcherMachines.map((machine) => {
                const key = machineKey(machine.conn);
                const tally = tallies.get(key);
                const name = machineLabel(machine.conn);
                // A MACHINE THAT IS NOT ANSWERING IS NOT A PLACE TO GO. Its tile used
                // to scope the whole screen to a machine with nothing to show, under
                // the word "offline" — the only label here that grew when its machine
                // got worse. Drained, it is dropped from `All`, and the press is the
                // one thing that machine can still do: ask it again. The name and the
                // transport's own reason ride the title, where the block cannot speak.
                const isDown = Boolean(machine.error);
                const retry = isDown ? retries.get(key) : undefined;
                return (
                  <MachineTab
                    key={key}
                    isOn={scope === key}
                    hasUnread={!isDown && (tally?.unread ?? 0) > 0}
                    isDown={isDown}
                    note={
                      retry === 'busy'
                        ? 'reconnecting...'
                        : retry === 'failed'
                          ? 'Unable to connect'
                          : null
                    }
                    isNoteError={retry === 'failed'}
                    label={isDown ? `Reconnect to ${name}` : undefined}
                    title={isDown ? `${name} is not answering — ${machine.error}` : undefined}
                    onClick={() => (isDown ? void retryMachine(machine.conn) : selectScope(key))}
                  >
                    <MachineMark color={machineColor(machineColors, key)} isHollow={isDown} />
                    {name}
                  </MachineTab>
                );
              })}
            </MachineSwitcher>
          </div>
          )}
          {/* WHAT IS WAITING TO BE SENT, on the row that already reports the
              state of this list. A share arrives with a payload and no
              destination, and the app must not guess: the memo the human sent
              from Messages belongs to a conversation only they can name. So the
              list says what is parked and stays a list — tapping a row sends it
              there, the yellow + on any project header sends it to a session
              that does not exist yet, and the ✕ throws it away. */}
          {share && (
            <div className="order-last w-full">
              <Banner
                kind="neutral"
                title="Sharing"
                dismiss={
                  onDiscardShare
                    ? { label: 'Discard the share', onClick: onDiscardShare }
                    : undefined
                }
              >
                {shareSummary(share)} — pick a session, or start a new one
              </Banner>
            </div>
          )}
          {/* THE SEARCH REPORT IS A LINE OF ITS OWN ON A PHONE. It used to ride the
              trailing cluster beside the switch on a row that could not shrink, so on a
              390px glass "271 matches / 1 machine did not answer" pushed the strip until
              the machine's own address was cut mid-token (`100.109.18.77:78`) and the
              report itself ran past the 12px inset to the edge of the screen. The row
              WRAPS instead: the switch keeps the whole first line, the report takes the
              second one whole, and from `sm` up — where there is room for both — it goes
              back inline at the trailing end. */}
          {searching && sessions !== null && (
            <div className="order-last flex w-full min-w-0 flex-wrap items-center gap-x-2 gap-y-1 sm:order-none sm:w-auto sm:flex-nowrap">
              {/* A filter is a FLEET question, and the count it came back with is the
                  only proof it left this gateway. It is the one fact this row reports:
                  totals were the same numbers the project headers below already carry.
                  It counts the ROWS ON SCREEN, so it is spoken only once a needle has
                  actually been asked: inside a pause the list is still the last answer,
                  and before the first one there is nothing filtered to count. */}
              {searched && (
                <span className="whitespace-nowrap font-mono text-chip font-bold text-accent-ink">
                  {searchCounts.matches} {searchCounts.matches === 1 ? 'match' : 'matches'}
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
                      {searchUnreached.size}{' '}
                      {searchUnreached.size === 1 ? 'machine' : 'machines'} did not answer
                    </span>
                  )}
                </>
              )}
            </div>
          )}
          <div className="flex shrink-0 items-center gap-2">
            {/* Only when no button can speak for it: a create started from this row's
                own menu belongs to no project header. Every header-started create
                wears its word INSIDE the button that was pressed. */}
            {creating && creating.at === null && (
              <span aria-live="polite" className="font-mono text-chip text-dialog-hint">
                {creating.label}
              </span>
            )}
            {/* The machine's own control, on the row that names that machine: its
                PROJECTS — choose the current one, add one, remove one.

                It is the amber primary, the same fill the list's create verb wears,
                and it is a MARK: the folder this app uses for a place on disk. It used
                to say `New project` beside `New session` one row below, two amber
                paragraphs promising two different creations, and only one of them was
                a create at all.

                In the fleet view it rides each machine's own band instead: there is no
                one machine for this row to speak for, and a control that had to ask
                which computer it meant would be the chooser the switch exists to
                abolish. */}
            {!isDesk && projectsVerb}
          </div>
        </div>
      )}
      {/* TWO COLUMNS ONLY WHERE THERE ARE TWO COLUMNS. `contents` is not a layout:
          at every other size this wrapper is not there at all and the card keeps the
          section's own single column, exactly as it had it. */}
      <div className={showRail ? 'flex min-h-0 flex-1' : 'contents'}>
        {showRail && (
          <FleetRail
            machines={railMachines}
            projects={railProjects}
            action={projectsVerb}
          />
        )}
        {/* ON A PHONE THE CARD IS THE PAGE, AND IT DOES NOT BREATHE.
            It used to be `mx-3` with a full box and a height that followed its content,
            so every page of the pager resized the frame under the finger (page 74 has 1
            row) and the whole screen jumped; its two side rules also stole 12px of a
            390px glass for nothing. Full bleed, no vertical rules, and `h-full` keep the
            frame fixed while the rows scroll inside it. The final machine closes the list with
            one neutral 2px rule exactly where its content ends; machine identity stays in the
            switcher instead of becoming another frame around every section. The section's own
            `env(safe-area-inset-bottom)` keeps that edge and the final row clear of the home
            indicator.
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
        <div className="relative flex h-full min-h-0 flex-col overflow-hidden border-t border-dialog-edge bg-page sm:max-h-full sm:border-0">
        {/* The pull reports itself where the search door lives: it takes over the app bar
            until the finger releases, instead of inserting a new band above the list. */}
        <PullToSearchHint phase={pullPhase} ref={hintRef} />
        {/* A create that failed has no button left to speak from once the order's own
            popover is gone, so the word lands on the paper the list is about to fill. */}
        {createError && (
          <div className="border-b border-dialog-edge bg-panel-2 px-3 py-2 sm:px-4">
            <Banner kind="err">{createError}</Banner>
          </div>
        )}

        <div ref={listRef} className="min-h-0 flex-1 touch-pan-y overflow-x-hidden overflow-y-auto overscroll-contain [overflow-anchor:auto] [scrollbar-gutter:stable] pb-3 sm:px-3">
        {/* A PROMOTION WAITS FOR THE READER, and the arrow points UP because that
            is where those rows go. Rows fresher than the oldest row on screen are
            counted here instead of being inserted under the thumb; the tap is the
            reader saying when. */}
        {pendingCount > 0 && (
          <LoadMore
            label={`Show ${pendingCount} newer ${pendingCount === 1 ? 'session' : 'sessions'}`}
            onClick={() => {
              adopt();
              listRef.current?.scrollTo({ top: 0 });
            }}
          >
            {pendingCount === 1 ? '1 newer session' : `${pendingCount} newer sessions`}
          </LoadMore>
        )}
        {sessions === null ? (
          <NavigatorSkeleton />
        ) : visible?.length === 0 && sections.every(({ groups }) => groups.length === 0) ? (
          <div className="px-5 py-16 text-center">
            {/* A query whose answer has not come back yet is not a dead end, and
                saying "No matching sessions" while every gateway is still reading
                its transcripts is the screen lying about a result it does not
                have. Outside search, this state means there is NO PROJECT: an empty
                project still renders its own header and the New session action it owns. */}
            <p className="font-mono text-body font-bold text-white/70">
              {searchPending ? 'Searching...' : query ? 'No matching sessions' : 'No projects yet'}
            </p>
            <p aria-live="polite" className="mt-2 font-mono text-ui text-dialog-hint">
              {searchPending
                ? searchAsked.length > 1
                  ? `Read ${searchAnswered.size} of ${searchAsked.length} machines so far.`
                  : 'Reading this machine’s transcripts.'
                : query
                  ? searchVerdict
                  : 'Add a project to start a session.'}
            </p>
            {/* The field is in the app bar now, a screen away from this sentence, so the
                way back to a full list is offered where the dead end is. A search still
                in flight has no dead end to offer it for. */}
            {query && !searchPending && (
              <div className="mt-4 flex justify-center">
                <Button variant="secondary" onClick={() => onQuery('')}>
                  Clear search
                </Button>
              </div>
            )}
          </div>
        ) : (
          <div>
            {sections.map(({ machine, groups, reading }, sectionIndex) => {
              const key = machineKey(machine.conn);
              return (
                <section
                  key={key}
                  aria-label={`${machineLabel(machine.conn)} projects`}
                  className={
                    sectionIndex === sections.length - 1
                      ? 'border-b-2 border-dialog-edge'
                      : undefined
                  }
                >
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
                  {groups.length === 0
                    ? (
                        <div className="px-3 py-3 sm:px-4">
                          <p className="font-mono text-meta text-dialog-hint">
                            {machine.sessions === null
                              ? 'Reading sessions...'
                              : searching
                                ? searchUnreached.has(key)
                                  ? 'Could not reach this machine.'
                                  : searchAnswered.has(key)
                                    ? 'No matches on this machine.'
                                    : 'Searching this machine...'
                                : 'No projects on this machine yet.'}
                          </p>
                        </div>
                      )
                    : groups.map((group, groupIndex) => (
                        // Nothing separates two projects: the band that opens the next
                        // one brings its own paper and its own rule in over the name.
                        <ProjectGroup
                          key={`${key}\u0000${group.root}`}
                          group={group}
                          machine={machine}
                          context={rowContext}
                          reading={reading}
                          creation={projectCreation}
                          // The order already put the machine's live work on top; the
                          // project it lands on is the one that opens by itself.
                          initiallyOpen={groupIndex === 0}
                        />
                      ))}
                </section>
              );
            })}
          </div>
        )}
        </div>

        {/* Only the WAIT is left here. The fraction moved into the filter band, which
            is where the filtering happens — printing "708 of 970" in a footer while
            the control that produced it said nothing was the same fact in the wrong
            place, and the third copy of it on the screen. */}
        {sessions === null && (
          <footer className="hidden items-center justify-end border-t border-dialog-edge bg-panel-2 px-3 py-2 font-mono text-meta text-dialog-hint sm:flex sm:bg-page sm:px-4">
            <span>Reading sessions...</span>
          </footer>
        )}
        {/* THE DESK'S OWN FOOTER, and it says only what is true here: `/` opens the
            fleet search (`App`), and both counts are the gateway's. A window this size
            can hold a footer without spending a row of the list on it. */}
        {isDesk && (
          <footer className="flex items-center justify-between gap-3 border-t border-dialog-edge bg-panel-2 px-4 py-1.5 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint sm:bg-page">
            <span className="flex items-center gap-1.5">
              <kbd className="border border-dialog-edge px-1 font-mono text-chip normal-case">/</kbd>
              Search the fleet
            </span>
            {/* WITH NO RAIL, THE FOOTER CARRIES WHAT THE RAIL CARRIED: which machine
                these rows came from, and the one door to that machine's projects. */}
            <span className="flex items-center gap-3">
              {!showRail && projectsVerb}
              <span className="tabular-nums">
                {soleMachine
                  ? `${machineLabel(soleMachine.conn)} · ${fleetSessions} ${fleetSessions === 1 ? 'session' : 'sessions'}`
                  : `${fleetSessions} sessions · ${machines.length} machines`}
              </span>
            </span>
          </footer>
        )}
      </div>
      </div>

       {/* Conversation fork choices, anchored under the row action that opened them. */}
      {forkFlow && (
        <Menu label="Fork this session" at={forkFlow.at} onDismiss={() => leaveFork(true)}>
          <MenuHeading>
            Fork · {forkFlow.session.title?.trim() || 'Untitled session'}
          </MenuHeading>
          <MenuItem
            title="The whole session"
            hint="A new session carrying every turn of this one, continuing from the end. This session stays exactly as it is."
            badge={forkBusy === WHOLE_SESSION_FORK ? 'Forking...' : 'Default'}
            onSelect={() => void runFork()}
          />
          <MenuHeading tone="quiet">Or fork at a turn</MenuHeading>
          {forkPoints.rows === null ? (
            <MenuNote>
              <Spinner tone="accent" />
              Reading turns...
            </MenuNote>
          ) : forkPoints.rows.length === 0 ? (
            <MenuNote>{forkPoints.error ?? 'This session has no turns to fork yet.'}</MenuNote>
          ) : (
            <>
              {forkPoints.error && <MenuNote>{forkPoints.error}</MenuNote>}
              {forkPoints.rows.map((point, index) => (
                <MenuItem
                  key={point.turn_id}
                  title={`Turn ${index + 1} · ${firstLine(point.request ?? '') || 'No words on this turn'}`}
                  hint="The fork keeps this turn and everything before it, and nothing after."
                  badge={forkBusy === point.turn_id ? 'Forking...' : undefined}
                  onSelect={() => void runFork(point.turn_id)}
                />
              ))}
            </>
          )}
        </Menu>
      )}


      {manageProjects && (
        <ManageProjectsSheet
          label={machineLabel(manageProjects.machine.conn)}
          at={manageProjects.at}
          client={clientFor(manageProjects.machine.conn)}
          startAt={machineProject(manageProjects.machine)?.path ?? null}
          knownRoots={new Set(
            projectGroups(
              manageProjects.machine.overview,
              manageProjects.machine.sessions ?? [],
            )
              .map((group) => group.root)
              .filter(Boolean),
          )}
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

    </section>
  );
}


/** How many rows one purge walk asks for at a time — a read nobody is watching. */
const PURGE_WALK = 200;

/**
 * Every session id in one project, read the way the list reads its pages.
 *
 * A group with no saved project row can only be purged session by session, and this
 * device holds a window of the MACHINE, never the project — so the ids are walked from
 * the gateway at the moment the purge is confirmed, and the fan-out is over what the
 * gateway says the root holds instead of over what happened to be on screen.
 */
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
  return current.length === next.length && current.every((session, index) => session === next[index])
    ? current
    : next;
}
