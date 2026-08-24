import { Fragment, memo, useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState } from 'react';
import {
  Banner,
  Button,
  CloseButton,
  ConfirmRow,
  DialogFrame,
  HeaderActions,
  HeaderMeta,
  HeaderTally,
  HeaderTitle,
  Pager,
  Input,
  LIST_EDGE,
  LIST_EDGE_END,
  LIST_FRAME,
  ProjectStatusCounts,
  LoadMore,
  MachineMark,
  MachineRail,
  MachineSwitcher,
  MachineTab,
  Modal,
  MachineProjectsButton,
  NewSessionButton,
  ProjectCrumb,
  PullToSearchHint,
  RowDisclosure,
  SectionGap,
  SectionHeader,
  Spinner,
} from '../components/ui';
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
import type { ForkPoint, GatewayConn, Session, SessionUsage } from '../lib/types';
import { compactProjectPath } from '../lib/path';
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
import {
  EPOCH_STALE_AWAY_MS,
  holdOrder,
  useOrderEpoch,
  type OrderEpoch,
} from '../lib/order-epoch';
import { usePullToSearch, type PullPhase } from '../lib/pull-to-search';
import { SwipeActions } from '../components/SwipeActions';
import {
  ManageProjectsSheet,
  type ManagedProject,
} from '../components/ManageProjectsSheet';
import {
  ForkIcon,
  PencilIcon,
  StarIcon,
  TrashIcon,
} from '../components/icons';
import { useFitRows, useMouseDensity } from '../lib/fit-rows';
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
  EMPTY_DRAFT_MESSAGE,
  flushDraftMessages,
  useDraftMessages,
  type DraftMessage,
  type DraftMessageStore,
} from '../lib/draft-messages';
import type { PendingAttachment } from '../lib/attachments';
import { shareSummary, type SharedPayload } from '../lib/share-intake';
import { favoriteRank, isFavorite, nextFavoriteRank } from '../lib/favorites';
import {
  fleetError,
  isFleetLoaded,
  machineCounts,
  machineKey,
  machineLabel,
  projectGroups,
  searchGroups,
  type Tally,
  reconcileMachines,
  resolveScope,
  sameOverview,
  scopedMachines,
  SEARCH_UNPLACED,
  searchFanout,
  searchOrder,
  searchTally,
  sessionIsLive,
  sessionNeedsInput,
  sessionMillis,
  sessionOrder,
  timeLabel,
  withSearchHits,
  machineProject,
  type FleetMachine,
} from '../lib/fleet';
import { projectFoldKey, readProjectFold, writeProjectFold } from '../lib/project-fold';

const SESSION_LIST_EVENTS = new Set([
  'turn.started',
  'turn.completed',
  'turn.failed',
  'turn.cancelled',
  'session.title_updated',
  // A run PARKED on a human ends no turn and streams nothing, so without these
  // two the list kept painting a plain LIVE row for a session that was already
  // waiting on the reader — and kept it there after somebody answered.
  'human_input.request',
  'human_input.close',
]);

// A background poll issued right before the OS suspended the webview can never
// settle: it neither resolves nor rejects after the resume. A plain in-flight
// boolean would then stay latched forever and every later refresh would be
// skipped — the list froze until the app was restarted. Anything older than
// this is treated as lost.
const STALE_POLL_MS = 20_000;

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

// A page whose read has not answered yet paints nothing rather than rows from
// another place in the project (`ProjectGroup`).
const NO_ROWS: Session[] = [];

/**
 * How many pages ahead of the one on screen a project group reads.
 *
 * Two, not three: the third is rarely reached and costs a full round trip and a
 * validator to hold. Nothing is read ahead until the visible page has answered.
 */
const PAGES_AHEAD = 2;
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
const SKELETON_GROUPS = [
  ['w-3/5', 'w-2/5', 'w-1/2'],
  ['w-1/2', 'w-2/3'],
];

// One client per paired machine, kept for the life of the tab. The snapshot
// cache and the conditional-GET validators live on the instance, so rebuilding
// one per poll would refetch the whole fleet every ten seconds.
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
 *   - a session row is 48px with a 1px rule under it, and 34px + 1px under
 *     `mouse:`, where the density follows the pointer (`index.css`);
 *   - the first row of a project starts at y=211 (y=215 under `mouse:`) — the app
 *     bar, the filter row, the scope strip, the project's own band and the shelf
 *     carrying its pager, all of which a page pays for before its first row;
 *   - PEEK is what a page leaves UNDER its last row, so the next project's band
 *     shows and the list never ends flush with the bottom of the screen.
 *
 * Three rows is the shortest page that still reads as a list rather than as a
 * pager with a row attached.
 */
const LIST_PEEK = 40;
const LIST_GEOMETRY = {
  touch: { row: 49, chrome: 211 + LIST_PEEK, min: 3 },
  mouse: { row: 35, chrome: 215 + LIST_PEEK, min: 3 },
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
  return useFitRows(isMouse ? LIST_GEOMETRY.mouse : LIST_GEOMETRY.touch);
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
  // A machine OWNS its projects: every row belongs to exactly one gateway, and a
  // project only exists inside the machine it lives on. The fleet is therefore
  // one entry per paired machine, seeded from that machine's last known list so
  // returning to this tab repaints the previous frame instantly; the effects
  // below revalidate each machine independently and reconcile on top.
  const [machines, setMachines] = useState<FleetMachine[]>(() => hydrateMachines(conns, []));
  // Exactly one paired machine is always active. Seed the state with the first machine
  // itself — not an unselected sentinel — and resolve removals or failures to the next
  // machine that can answer. Pressing the selected tab cannot turn it off.
  const [scopePick, setScopePick] = useState<string | null>(() =>
    conns[0] ? machineKey(conns[0]) : null,
  );
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
  // The verbs the slide uncovers on a row, plus the group header's project delete. One dialog
  // serves all three: renaming asks for the new title, both deletes ask for consent
  // — a destructive tap two pixels from a thumb rest position must never be one-way.
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
  const [rowAction, setRowAction] = useState<RowAction | null>(null);
  const [renameDraft, setRenameDraft] = useState('');
  const [actionBusy, setActionBusy] = useState(false);
  const [actionError, setActionError] = useState<string | null>(null);
  // Fan-out progress. Deleting a group that is not a project row is one request per
  // session, and forty of them behind a motionless 'Deleting...' is indistinguishable
  // from a hang.
  const [actionProgress, setActionProgress] = useState<{ done: number; total: number } | null>(null);
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
    for (const machine of machines) if (machine.sessions) seedReadMarks(machine.sessions);
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
          // Parked rows and stable project totals arrive BESIDE the head window.
          // Adopt all three in one patch so no intermediate frame tallies whichever
          // session pages happened to land first.
          const parked = reconcileSessions(held?.awaiting ?? null, api.parkedSessions());
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
            parked === held.awaiting &&
            overview === held.overview
          )
            return;
          patchMachine(key, (machine) => ({
            ...machine,
            sessions: merged,
            awaiting: parked,
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
      void load(controller.signal, true);
    };

    void load(controller.signal);
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

  useEffect(() => {
    // Same rule as the poll above: a lifecycle event cannot move a list nobody is
    // looking at, and the load on becoming visible answers with the gateway's
    // canonical order anyway.
    if (!subscriptions || !isVisible) return;
    let refreshTimer: number | null = null;
    const unsubscribe = subscriptions.subscribeFleet((event) => {
      if (!SESSION_LIST_EVENTS.has(event.type)) return;
      if (refreshTimer !== null) window.clearTimeout(refreshTimer);
      // Coalesce lifecycle bursts, then ask the gateway for its canonical order.
      refreshTimer = window.setTimeout(() => void load(undefined, true), 120);
    });
    return () => {
      unsubscribe();
      if (refreshTimer !== null) window.clearTimeout(refreshTimer);
    };
  }, [isVisible, load, subscriptions]);

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

  // THE DEMAND IS PINNED, NEVER LIFTED.
  //
  // A run parked on an unanswered human-input request is the one state a reader cannot
  // infer and only they can clear: the turn is live, nothing is streaming, and it will
  // stay that way until they answer it. It used to LEAD the gateway's ordering key,
  // which moved every row on the screen whenever a turn asked for a human or got its
  // answer — and, because the key is applied before the page is cut, pushed another
  // session out of the window a reader was paging. So the gateway now answers those
  // rows BESIDE the window (`GatewayClient.parkedSessions`), complete however deep in
  // the fleet they sit, and they are pinned in their own band above a list whose order
  // never flinches.
  //
  // A SEARCH IS THE READER'S OWN QUESTION and this band is not part of the answer, so
  // it stands down while a query is live.
  const parked = useMemo(
    () =>
      searchNeedle
        ? []
        : filtered.flatMap((entry) =>
            (entry.machine.awaiting ?? []).map((session) => ({
              session,
              conn: entry.machine.conn,
            })),
          ),
    [filtered, searchNeedle],
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
  // only a destination list: keep each group's original order, but put every destination
  // that no longer answers after all the destinations the reader can still enter.
  const switcherMachines = useMemo(
    () => [
      ...machines.filter((machine) => !machine.error),
      ...machines.filter((machine) => Boolean(machine.error)),
    ],
    [machines],
  );

  const selectScope = useCallback((next: string | null) => setScopePick(next), []);

  // ONE sheet, opened from wherever the machine is named: the row above the card when
  // the list is scoped, and that machine's own band in the fleet view. It is anchored
  // on the button that was pressed, so the anchor travels with the verb.
  const openManageProjects = useCallback((machine: FleetMachine, anchor: HTMLElement) => {
    const at = menuPosition(anchor.getBoundingClientRect(), BROWSE_WIDTH);
    if (!at) return;
    setManageProjects({ machine, at });
  }, []);


  async function createSession(on: GatewayConn, root: string) {
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
  }


  const startRename = useCallback((session: Session, conn: GatewayConn) => {
    setRowAction({ mode: 'rename', session, conn });
    setRenameDraft(session.title?.trim() ?? '');
    setActionError(null);
  }, []);

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

  const startDelete = useCallback((session: Session, conn: GatewayConn) => {
    setRowAction({ mode: 'delete', session, conn });
    setActionError(null);
  }, []);

  // The unit is the group ON THIS MACHINE, never "this project everywhere": the same
  // repo checked out on two machines is two projects and two deletes.
  const startProjectDelete = useCallback((project: ManagedProject, conn: GatewayConn) => {
    setRowAction({
      mode: 'project',
      project: project.name,
      root: project.root,
      projectId: project.projectId,
      count: project.count,
      live: project.live,
      conn,
    });
    setActionError(null);
    setActionProgress(null);
  }, []);

  // Dismissable even mid-request. A delete is already on the wire and cannot be
  // taken back, but the SCREEN must always come back: a confirm dialog that
  // refuses to close until the gateway answers reads as a frozen app (and with
  // an unreachable machine it stayed up for the full request timeout).
  const cancelDelete = useCallback(() => {
    setRowAction(null);
    setActionError(null);
    setActionProgress(null);
  }, []);

  function closeRowAction() {
    cancelDelete();
  }

  async function commitRowAction() {
    if (!rowAction) return;
    const api = clientFor(rowAction.conn);
    const key = machineKey(rowAction.conn);
    // The words that made a row dirty die with it: a draft message kept under a
    // session id that no longer exists is unreachable forever. The star needs no
    // sweep of its own — it lived on the session the gateway just deleted.
    const forgetDraftMessages = (ids: string[]) => {
      for (const sid of ids) clearDraftMessage(draftMessageKey(api.base, sid));
      if (ids.length > 0) void flushDraftMessages();
    };
    // A finished row action is not news to go and FETCH. The gateway has already
    // answered — which ids died, or the row it stored — so the new list is the old
    // one with that answer applied, on THIS machine, because no other one was
    // touched.
    //
    // Regression, user report (paraphrased: removing a single session re-downloaded
    // every session): this used to end in `load()`, a full walk of every 100-row
    // window on every paired machine (~728KB over 11 serial round trips on a
    // 1100-session gateway, see `createSession`) to be told one id that was already
    // in hand — with the confirm standing over the list until the whole fleet
    // drained. Deleting one row costs no request beyond the DELETE itself.
    //
    // Returning the SAME array means nothing changed, which bails `patchMachine`
    // out before any row re-renders.
    const patchRows = (update: (rows: Session[]) => Session[]) =>
      patchMachine(key, (machine) => {
        const rows = machine.sessions;
        if (!rows) return machine;
        const next = update(rows);
        return next === rows ? machine : { ...machine, sessions: next };
      });
    const forgetRows = (ids: string[]) => {
      if (ids.length === 0) return;
      const gone = new Set(ids);
      patchRows((rows) => {
        const kept = rows.filter((row) => !gone.has(row.id));
        return kept.length === rows.length ? rows : kept;
      });
    };
    const title = renameDraft.trim();
    if (rowAction.mode === 'rename' && !title) {
      setActionError('A session name cannot be empty.');
      return;
    }
    setActionBusy(true);
    setActionError(null);
    try {
      if (rowAction.mode === 'rename') {
        // The gateway echoes the row it stored, so the new name arrives WITH the
        // answer. Keyed by the id we asked about and with the requested title left
        // standing under the echo, so a thin answer still repaints the row rather
        // than leaving the old name up until the next poll. Ordering is deliberately
        // untouched: a row that jumps out from under the thumb the instant it is
        // named reads as a bug, and the poll re-ranks it soon enough.
        const sid = rowAction.session.id;
        const renamed = await api.renameSession(sid, title);
        patchRows((rows) =>
          rows.some((row) => row.id === sid)
            ? rows.map((row) => (row.id === sid ? { ...row, title, ...renamed, id: sid } : row))
            : rows,
        );
      } else if (rowAction.mode === 'delete') {
        await api.deleteSession(rowAction.session.id);
        forgetDraftMessages([rowAction.session.id]);
        forgetRows([rowAction.session.id]);
      } else {
        // ONE REQUEST WHERE THE GATEWAY OWNS THE PROJECT: it deletes the members it
        // knows about, which is more than any list paints.
        if (rowAction.projectId) {
          const deleted = await api.deleteProject(rowAction.projectId);
          forgetDraftMessages(deleted);
          forgetRows(deleted);
        } else {
          // No project row to hand the group to, so the fan-out IS the delete — over
          // the ids the GATEWAY says the root holds, walked at the moment the purge is
          // confirmed, because this device holds a window and never the project. It
          // keeps going past a failure and says what survived, instead of stopping half
          // way with nothing said.
          const ids = await projectSessionIds(api, rowAction.root);
          const gone: string[] = [];
          let failed = 0;
          setActionProgress({ done: 0, total: ids.length });
          for (const sid of ids) {
            try {
              await api.deleteSession(sid);
              gone.push(sid);
            } catch {
              failed += 1;
            }
            setActionProgress({ done: gone.length + failed, total: ids.length });
          }
          forgetDraftMessages(gone);
          // A partial fan-out is exactly known too: the ids that died leave, the ones
          // that refused keep their rows, and the note says how many refused.
          forgetRows(gone);
          if (failed > 0) {
            setActionError(`${failed} of ${ids.length} sessions could not be deleted.`);
            return;
          }
        }
      }
      setRowAction(null);
    } catch (cause) {
      setActionError((cause as Error).message);
    } finally {
      setActionBusy(false);
      setActionProgress(null);
    }
  }

  const rowCopy = rowAction ? rowActionCopy(rowAction, machineLabel(rowAction.conn)) : null;

  // Deleting ONE session is confirmed IN the row, so the confirm has to reach
  // `commitRowAction` from inside a memoised row. Through a ref, not a fresh
  // closure per paint: that would re-render every row of a 700-row list on
  // every poll.
  const commitRef = useRef<() => void>(() => {});
  commitRef.current = () => void commitRowAction();
  const confirmDelete = useCallback(() => commitRef.current(), []);

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
        admitted: entry.admitted,
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
    [heldRows, searching, readMarks],
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

  if (loadError) return null;

  return (
    <section aria-label="Sessions" className="mx-auto flex h-full min-h-0 w-full max-w-[1400px] flex-col pb-[env(safe-area-inset-bottom)] pl-[env(safe-area-inset-left)] pr-[env(safe-area-inset-right)] pt-0 transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none sm:px-6 sm:pb-6 sm:pt-6">
      {/* On phones this panel sits FLUSH under the app header, whose own `border-b`
          already draws the rule below the Vis mark. A `border-y` here stacked a
          second hairline on top of it, so the Sessions tab wore a 2px seam while
          Machines (which floats its cards below a gap) wore 1px. Bottom edge only;
          the full box comes back once the panel detaches at `sm`. */}
      {/* The card owns three sides. Its LEFT edge belongs to whatever is standing
          there — a neutral 2px rule under the chrome bands, the machine's own hue
          down everything that machine owns — because a rail beside a border is two
          lines doing one job, and a rail that is a BORDER also steals 2px of layout
          the trailing edge has no match for. Both sides are 2px now, so the ink
          lands symmetrically whichever one is painting. */}
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
      {machines.length > 0 && (
        <div className="relative z-10 flex flex-wrap items-center gap-x-1.5 gap-y-2 px-3 pb-3 pt-6 sm:flex-nowrap sm:pb-4 sm:pl-0 sm:pr-4 sm:pt-8">
          {/* The switch owns the leading space of this row: it GROWS, so the machine's
              verb stands at the trailing inset without an auto margin that would fight
              the search report for the same free space. The track inside it keeps its
              own compact width and scrolls a fleet that outgrows the row. */}
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
          {/* WHAT IS WAITING TO BE SENT, on the row that already reports the
              state of this list. A share arrives with a payload and no
              destination, and the app must not guess: the memo the human sent
              from Messages belongs to a conversation only they can name. So the
              list says what is parked and stays a list — tapping a row sends it
              there, the yellow + on any project header sends it to a session
              that does not exist yet, and the ✕ throws it away. */}
          {share && (
            <div className="order-last flex w-full min-w-0 items-center gap-2 sm:order-none sm:w-auto">
              <span className="whitespace-nowrap font-mono text-chip font-bold text-accent-ink">
                Sharing
              </span>
              <span className="min-w-0 flex-1 truncate font-mono text-chip text-dialog-hint">
                {shareSummary(share)} — pick a session, or start a new one
              </span>
              {onDiscardShare && (
                <CloseButton label="Discard the share" onClick={onDiscardShare} />
              )}
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
            {scopeMachine && !scopeMachine.error && (
              <MachineProjectsButton
                machine={machineLabel(scopeMachine.conn)}
                onPress={(anchor) => openManageProjects(scopeMachine, anchor)}
              />
            )}
          </div>
        </div>
      )}
        {/* ON A PHONE THE CARD IS THE PAGE, AND IT DOES NOT BREATHE.
            It used to be `mx-3` with a full box and a height that followed its content,
            so every page of the pager resized the frame under the finger (page 74 has 1
            row) and the whole screen jumped; its two side rules also stole 12px of a
            390px glass for nothing. Full bleed, no vertical rules, and `h-full` so the
            frame is fixed and the rows scroll inside it. The section's own
            `env(safe-area-inset-bottom)` keeps the last row and the pager clear of the
            home indicator, which used to swallow them.
            At `sm` the card detaches again and ENDS where its content ends (`max-h-full`
            + `h-auto`), so the desktop never draws a border around empty paper. */}
        <div className="relative flex h-full min-h-0 flex-col overflow-hidden border-y border-dialog-edge bg-panel sm:mx-0 sm:h-auto sm:max-h-full sm:border-x sm:border-r-2">
        {/* The pull reports itself while it happens: the card clips the band until a
            finger brings it down over the list's first header. */}
        <PullToSearchHint phase={pullPhase} ref={hintRef} />
        {/* A create that failed has no button left to speak from once the order's own
            popover is gone, so the word lands on the paper the list is about to fill. */}
        {createError && (
          <div className={`border-b border-dialog-edge bg-panel-2 px-3 py-2 sm:px-4 ${LIST_FRAME}`}>
            <Banner kind="err">{createError}</Banner>
          </div>
        )}

        <div ref={listRef} className="min-h-0 flex-1 touch-pan-y overflow-x-hidden overflow-y-auto overscroll-contain [overflow-anchor:auto] [scrollbar-gutter:stable]">
        {/* Pinned above the list, and it does not scroll away with a machine's
            section: the demand belongs to the whole fleet in view. */}
        <NeedsYou
          rows={parked}
          drafts={draftMessages}
          matches={matches}
          needle={searchNeedle}
          onOpen={onOpen}
          onRename={startRename}
          onFork={startFork}
          onDelete={startDelete}
          onToggleStar={toggleStar}
          rowAction={rowAction}
          deleteBusy={actionBusy}
          deleteError={actionError}
          onConfirmDelete={confirmDelete}
          onCancelDelete={cancelDelete}
        />
        {/* A PROMOTION WAITS FOR THE READER, and the arrow points UP because that
            is where those rows go. Rows fresher than the oldest row on screen are
            counted here instead of being inserted under the thumb; the tap is the
            reader saying when. */}
        {pendingCount > 0 && (
          <LoadMore
            isEarlier
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
        ) : visible?.length === 0 ? (
          <div className={`px-5 py-16 text-center ${LIST_FRAME}`}>
            {/* A query whose answer has not come back yet is not a dead end, and
                saying "No matching sessions" while every gateway is still reading
                its transcripts is the screen lying about a result it does not
                have. It was the whole report: no word about where the search was,
                then a list that arrived much later. */}
            <p className="font-mono text-body font-bold text-white/70">
              {searchPending ? 'Searching...' : query ? 'No matching sessions' : 'No sessions yet'}
            </p>
            <p aria-live="polite" className="mt-2 font-mono text-ui text-dialog-hint">
              {searchPending
                ? searchAsked.length > 1
                  ? `Read ${searchAnswered.size} of ${searchAsked.length} machines so far.`
                  : 'Reading this machine’s transcripts.'
                : query
                  ? searchVerdict
                  : 'Open the ⋯ menu to start one.'}
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
            {sections.map(({ machine, groups, admitted }, sectionIndex) => {
              const key = machineKey(machine.conn);
              const color = machineColor(machineColors, key);
              return (
                <section key={key} aria-label={`${machineLabel(machine.conn)} projects`}>
                  {/* Every machine keeps its own named panel and landmark, even when it is
                      the only machine in the fleet. */}
                  {/* Everything one machine owns hangs off ITS rail, and that rail IS
                      the card's left frame here — a project boundary is a hairline, a
                      machine boundary is a colour change, so where `tower` ends is seen
                      before it is read. The panel is always rendered for the machine
                      whose projects follow, fleet view or scoped view alike. */}
                  <MachineRail color={color}>
                  {/* Where one computer ends is a colour change AND a trough, so the
                      first project of the second machine can never read as the fifth
                      project of the first one. */}
                  {sectionIndex > 0 && <SectionGap />}
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
                                : 'No sessions on this machine yet.'}
                          </p>
                        </div>
                      )
                    : groups.map((group, groupIndex) => (
                        // Two projects used to be separated by the SAME hairline that
                        // separates two sessions of one project. Every group after the
                        // first opens on 8px of the machine's own paper instead.
                        <Fragment key={`${key}\u0000${group.root}`}>
                        {groupIndex > 0 && <SectionGap />}
                        <ProjectGroup
                          project={group.label}
                          root={group.root}
                          sessions={group.sessions}
                          tally={group.tally}
                          conn={machine.conn}
                          matches={matches}
                          needle={searchNeedle}
                          onOpen={onOpen}
                          onRename={startRename}
                          onFork={startFork}
                          onDelete={startDelete}
                          onToggleStar={toggleStar}
                          pendingDeleteId={
                            rowAction?.mode === 'delete' && machineKey(rowAction.conn) === key
                              ? rowAction.session.id
                              : null
                          }
                          deleteBusy={actionBusy}
                          deleteError={actionError}
                          onConfirmDelete={confirmDelete}
                          onCancelDelete={cancelDelete}
                           onNewSession={(root) => void createSession(machine.conn, root)}
                          creating={creating}
                          pageSize={pageSize}
                          epoch={epoch}
                          admitted={admitted}
                          isVisible={isVisible}
                          list={machine.sessions}
                          drafts={draftMessages}
                          // The order already put the machine's live work on top; the
                          // project it lands on is the one that opens by itself.
                          isTop={groupIndex === 0}
                        />
                        </Fragment>
                      ))}
                  </MachineRail>
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
          <footer className={`hidden items-center justify-end border-t border-dialog-edge bg-panel-2 px-3 py-2 font-mono text-meta text-dialog-hint sm:flex sm:px-4 ${LIST_FRAME}`}>
            <span>Reading sessions...</span>
          </footer>
        )}
      </div>

      {/* Renaming and the group purge keep the dialog. Deleting ONE session does
          not: its confirm is the row itself — see `SessionRow`. */}
      {rowAction && rowCopy && rowAction.mode !== 'delete' && (
        <Modal size="fit" onDismiss={closeRowAction}>
            <DialogFrame title={rowCopy.title} onClose={closeRowAction}>
              <div className="space-y-3 p-4">
                <p className="truncate font-mono text-meta text-dialog-hint">{rowCopy.subject}</p>
                {rowAction.mode === 'rename' ? (
                  <label className="block">
                    <span className="mb-1 block font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
                      Session name
                    </span>
                    <Input
                      autoFocus
                      value={renameDraft}
                      maxLength={200}
                      placeholder="Session name"
                      onChange={(event) => setRenameDraft(event.target.value)}
                      onKeyDown={(event) => {
                        if (event.key === 'Enter') void commitRowAction();
                      }}
                    />
                  </label>
                ) : (
                  <p className="font-mono text-body text-white">{rowCopy.body}</p>
                )}
                {rowCopy.live > 0 && (
                  <Banner kind="warn">
                    {rowCopy.live === 1
                      ? 'One of them is running right now and will be stopped.'
                      : `${rowCopy.live} of them are running right now and will be stopped.`}
                  </Banner>
                )}
                {actionError && <Banner kind="err">{actionError}</Banner>}
                <div className="flex justify-end gap-2">
                  <Button variant="secondary" onClick={closeRowAction}>
                    Cancel
                  </Button>
                  <Button
                    variant={rowAction.mode === 'rename' ? 'primary' : 'danger'}
                    disabled={actionBusy}
                    onClick={() => void commitRowAction()}
                  >
                    {actionBusy
                      ? rowAction.mode === 'rename'
                        ? 'Saving...'
                        : actionProgress
                          ? `Purging ${actionProgress.done} of ${actionProgress.total}...`
                          : rowAction.mode === 'project' ? 'Purging...' : 'Deleting...'
                      : rowAction.mode === 'rename'
                        ? 'Save'
                        : rowAction.mode === 'project' ? 'Purge' : 'Delete'}
                  </Button>
                </div>
              </div>
            </DialogFrame>
        </Modal>
      )}


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
          onRemove={(entry) => {
            const conn = manageProjects.machine.conn;
            setManageProjects(null);
            startProjectDelete(entry, conn);
          }}
        />
      )}

    </section>
  );
}

/** Rename one row, delete one row, or delete a whole group on ONE machine. */
type RowAction =
  | { mode: 'rename'; session: Session; conn: GatewayConn }
  | { mode: 'delete'; session: Session; conn: GatewayConn }
  | {
      mode: 'project';
      /** The header's own name and canonical root. */
      project: string;
      root: string;
      /** The gateway's project id, or `''` when the group is only a shared root. */
      projectId: string;
      /** What the GATEWAY says the group holds: the blast radius, not what is on screen. */
      count: number;
      live: number;
      conn: GatewayConn;
    };

/**
 * What the confirm says.
 *
 * A group delete states the FULL blast radius: the count is every session the gateway
 * tallies in the group, never the rows this device happens to hold, and it names the
 * machine — the same repo on two machines is two groups. It also never claims to
 * delete a project when the group is only a shared root.
 */
function rowActionCopy(
  action: RowAction,
  machine: string,
): { title: string; subject: string; body: string; live: number } {
  if (action.mode !== 'project') {
    return {
      title: action.mode === 'rename' ? 'Rename session' : 'Delete session',
      subject: `${action.session.title?.trim() || 'Untitled session'} · ${shortId(action.session.id)}`,
      body: 'Delete this session and its transcript from the gateway? This cannot be undone.',
      live: action.mode === 'delete' && sessionIsLive(action.session) ? 1 : 0,
    };
  }
  const sessions = `${action.count} ${action.count === 1 ? 'session' : 'sessions'}`;
  return {
    title: 'Purge sessions',
    subject: `${action.project} · ${machine}`,
    body: action.projectId
      ? `Purge all ${sessions} in this project, with every transcript, from ${machine}? This cannot be undone.`
      : `Purge all ${sessions} in this group, with every transcript, from ${machine}? They share a workspace but no saved project, so nothing else is removed. This cannot be undone.`,
    live: action.live,
  };
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


/**
 * THE BAND FOR RUNS PARKED ON A HUMAN — pinned above the list, never lifted into it.
 *
 * The navigator is ordered by content time and nothing else, so a session waiting on
 * its operator sits wherever it last spoke: in a long fleet, past the end of the window
 * this device has read. The band is where that demand is complete (the gateway answers
 * those rows beside the window — `GatewayClient.parkedSessions`), and it is the price
 * of taking liveness out of the ordering key: nothing moves under the reader any more,
 * and the one state only they can clear still costs no scrolling.
 *
 * The rows are the LIST's own rows, not a summary of them: the same `SessionRow`, the
 * same swipe, the same INPUT NEEDED mark. A parked session that IS in the window below
 * appears in both places, the way a pinned message does — its place in its project is
 * where the reader will look for it next time.
 */
const NeedsYou = memo(function NeedsYou({
  rows,
  drafts,
  matches,
  needle,
  onOpen,
  onRename,
  onFork,
  onDelete,
  onToggleStar,
  rowAction,
  deleteBusy,
  deleteError,
  onConfirmDelete,
  onCancelDelete,
}: {
  /** One entry per parked session, carrying the machine it is parked on. */
  rows: { session: Session; conn: GatewayConn }[];
  drafts: DraftMessageStore;
  matches: Map<string, SessionMatch> | null;
  needle: string;
  onOpen: Props['onOpen'];
  onRename: (session: Session, conn: GatewayConn) => void;
  onFork: (session: Session, conn: GatewayConn, anchor: HTMLElement) => void;
  onDelete: (session: Session, conn: GatewayConn) => void;
  onToggleStar: (session: Session, conn: GatewayConn) => void;
  /** The row anywhere in the fleet that is currently asking to be deleted, if any. */
  rowAction: RowAction | null;
  deleteBusy: boolean;
  deleteError: string | null;
  onConfirmDelete: () => void;
  onCancelDelete: () => void;
}) {
  if (rows.length === 0) return null;
  return (
    <section aria-label="Sessions waiting on you">
      {/* The band wears the warning hue as its outgoing rule, the same ink the rows
          below say INPUT NEEDED in, so the line under the name is read before it. */}
      <SectionHeader rule="border-warn-strong">
        <HeaderTitle name="Needs you" qualifier="Parked on an answer from you" />
        <HeaderActions>
          <HeaderMeta>
            <HeaderTally count={rows.length} unit="session" />
          </HeaderMeta>
        </HeaderActions>
      </SectionHeader>
      {rows.map(({ session, conn }) => {
        const pending =
          rowAction?.mode === 'delete' &&
          machineKey(rowAction.conn) === machineKey(conn) &&
          rowAction.session.id === session.id;
        return (
          <SessionRow
            key={`${machineKey(conn)}\u0000${session.id}`}
            session={session}
            draft={drafts[draftMessageKey(clientFor(conn).base, session.id)] ?? EMPTY_DRAFT_MESSAGE}
            conn={conn}
            match={matches?.get(session.id) ?? null}
            needle={needle}
            onOpen={onOpen}
            onRename={(row) => onRename(row, conn)}
            onFork={(row, anchor) => onFork(row, conn, anchor)}
            onDelete={(row) => onDelete(row, conn)}
            onToggleStar={(row) => onToggleStar(row, conn)}
            isConfirmingDelete={pending}
            deleteBusy={deleteBusy}
            deleteError={pending ? deleteError : null}
            onConfirmDelete={onConfirmDelete}
            onCancelDelete={onCancelDelete}
          />
        );
      })}
    </section>
  );
});

// Memoised: a 5.5s poll that changes nothing returns the SAME row objects
// (`reconcileSessions`), so an unchanged group must not re-render its rows.
const ProjectGroup = memo(function ProjectGroup({
  project,
  root,
  sessions,
  tally,
  conn,
  matches,
  needle,
  drafts,
  onOpen,
  onRename,
  onFork,
  onDelete,
  onToggleStar,
  pendingDeleteId,
  deleteBusy,
  deleteError,
  onConfirmDelete,
  onCancelDelete,
  onNewSession,
  creating,
  pageSize,
  epoch,
  admitted,
  isVisible,
  list,
  isTop,
}: {
  project: string;
  /** Canonical workspace root — the group's identity, and what its page is asked for by. */
  root: string;
  /**
   * What this device is HOLDING of that project: the rows of the machine's window
   * (`GatewayClient.listSessions`) that fall in it — never the project, which is read
   * a page at a time. It is what the group paints before its own page lands, and the
   * copy a star or a rename made here echoes out of.
   */
  sessions: Session[];
  /**
   * What this project HOLDS, as its gateway counted it (`projectTally`) — not
   * the rows this device has paged in, which are a window and read low until the
   * whole list has drained.
   */
  tally: Tally;
  conn: GatewayConn;
  matches: Map<string, SessionMatch> | null;
  needle: string;
  /** Unsent composer content for the whole fleet; each row reads its own entry. */
  drafts: DraftMessageStore;
  onOpen: Props['onOpen'];
  onRename: (session: Session, conn: GatewayConn) => void;
  onFork: (session: Session, conn: GatewayConn, anchor: HTMLElement) => void;
  onDelete: (session: Session, conn: GatewayConn) => void;
  onToggleStar: (session: Session, conn: GatewayConn) => void;
  /** The row of THIS machine that is currently asking to be deleted, if any. */
  pendingDeleteId: string | null;
  deleteBusy: boolean;
  deleteError: string | null;
  onConfirmDelete: () => void;
  onCancelDelete: () => void;
  onNewSession: (root: string) => void;
  /**
   * The create this very project header started, so its own button can say the word
   * instead of a label parked on the app bar saying it for the whole fleet.
   */
  creating: { at: string | null; label: string } | null;
  pageSize: number;
  /** The order this reader agreed to, which the page below is held in (`lib/order-epoch`). */
  epoch: OrderEpoch | null;
  /** Rows this reader is not surprised by: what they just started, what holds their words. */
  admitted: ReadonlySet<string>;
  /**
   * Whether this screen is on the glass. A group parked behind an open transcript
   * asks the gateway for nothing: its page is a READ, and reading a page nobody
   * can see is the cascade the fleet poll already refuses to run off the glass.
   */
  isVisible: boolean;
  /**
   * The machine's OWN list, taken by identity alone: a poll that changed nothing
   * hands back the very array it was handed before (`GatewayClient.listSessions`),
   * so a page is re-read exactly when the list under it moved — never once per poll
   * per project, and never on a re-render this device made for itself.
   */
  list: Session[] | null;
  /**
   * This is the project the machine's own order put ON TOP, and the one project
   * that opens without being asked. Everything below it starts folded.
   */
  isTop: boolean;
}) {
  const base = useMemo(() => clientFor(conn).base, [conn]);
  // Row actions must reach the machine that OWNS the row. Bound here so a
  // memoized row does not re-render on every paint of its parent.
  const renameRow = useCallback((session: Session) => onRename(session, conn), [onRename, conn]);
  const forkRow = useCallback(
    (session: Session, anchor: HTMLElement) => onFork(session, conn, anchor),
    [onFork, conn],
  );
  const deleteRow = useCallback((session: Session) => onDelete(session, conn), [onDelete, conn]);
  const starRow = useCallback(
    (session: Session) => onToggleStar(session, conn),
    [onToggleStar, conn],
  );

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
  // THE WINDOWS THIS GROUP HOLDS, and the validator each was issued under
  // (`GatewayClient.listProjectPage`). They used to live in a static map on the client,
  // capped at 24 for the whole fleet and evicted oldest-first: a budget shared by every
  // project of every machine, spent by a rule that could not know which page anybody
  // was looking at. A window belongs to whoever READS it — this store is read ahead
  // into, answered from, and forgotten with the group.
  const pins = useRef<ProjectWindows>(new Map());
  // The question the last read asked, so a page TURN paints what is already held and a
  // poll that only moved the list under an unchanged page repaints nothing.
  const asked = useRef('');
  // The page LAST ANSWERED, whichever it is: the rows and the project's own count.
  const [paged, setPaged] = useState<{ rows: Session[]; total: number } | null>(null);
  // A project FOLDS, and only the top one starts open: the screen's job is to show
  // the work that moved last, not four checkouts' history at once. What the reader
  // folds afterwards is theirs and outlives this component — see `lib/project-fold`.
  const foldKey = projectFoldKey(machineKey(conn), root);
  const [isOpen, setIsOpen] = useState(() => readProjectFold(foldKey) ?? isTop);
  // A fold is a DECISION, not a frame: it is written where it was made, so the next
  // screen built from nothing starts where this reader left it.
  const fold = (open: boolean) => {
    writeProjectFold(foldKey, open);
    setIsOpen(open);
  };
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
    const api = clientFor(conn);
    // A page this group already HOLDS paints in the frame of the tap that asked for
    // it. Only when the question changed: a poll that moved the list under an unchanged
    // page must not repaint it from a validator that is about to be revalidated anyway.
    const question = `${limit}\u0000${after}`;
    if (question !== asked.current) {
      asked.current = question;
      const held = api.heldProjectPage(root, limit, after, pins.current);
      if (held) setPaged({ rows: held.rows.slice(start - from), total: held.total });
    }
    void (async () => {
      try {
        const answer = await api.listProjectPage(
          root,
          limit,
          after,
          pins.current,
          control.signal,
        );
        if (!live) return;
        if (answer.nextCursor)
          cursors.current.set(from + answer.rows.length, answer.nextCursor);
        setPaged({
          rows: answer.rows.slice(start - from),
          total: answer.total,
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
  }, [conn, root, start, pageSize, isVisible, isShowing, searching, list]);
  // The count under the header and the pages beside it are ONE number — the
  // project's own total, as the gateway counted it. Under a query the complete
  // answer is on this device, and then what is on screen is the honest count.
  const total = searching ? sessions.length : (paged?.total ?? tally.count);
  const pageCount = Math.max(1, Math.ceil(Math.max(total, 1) / pageSize));
  const shownPage = Math.min(page, pageCount);
  // A PAGE ARRIVES OVER THE ONE BEFORE IT, NEVER OVER A HOLE. The read a step takes
  // lands a beat after the tap, and a group that painted nothing meanwhile lost its
  // rows, its height AND the pager the thumb had just pressed — the reflow this
  // whole seam exists to end. So the last page answered stays on the glass until the
  // next one lands, and only page ONE has something else to open on: what this device
  // holds of this project, out of the machine's own window.
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
    return searching
      ? shown
      : holdOrder(
          epoch,
          shown,
          (session) => ({ id: session.id, millis: sessionMillis(session) }),
          admitted,
        ).rows;
  }, [searching, painting, local, epoch, admitted]);
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
    rowsRef.current
      ?.querySelector(`[data-session-id="${CSS.escape(id)}"]`)
      ?.scrollIntoView({ block: 'nearest', inline: 'nearest', behavior: 'auto' });
  }, [rows]);
  return (
    <>
    <section aria-label={`${project} sessions`}>
      {/* The project band wears the accent as its outgoing rule — one band per
          project, and the yellow line under it says the rows below belong to the
          name above them rather than to the machine two bands up. */}
      <SectionHeader rule="border-accent">
        {/* The leading half NAMES the project and FOLDS it: folder name, the path that
            tells two `vis` checkouts apart UNDER it, and a chevron in the mark column
            the band already reserves, so the name keeps the list's one leading edge
            and the path gets the whole column instead of the crumbs of one. Paging
            walks a project's history; the fold decides whether it is on screen at
            all, which is what a reader with four checkouts on one machine needs. */}
        <ProjectCrumb
          name={project}
          qualifier={
            // The path says WHICH checkout this is, the count says how much of it
            // there is: one quiet line under the name, in the hint ink both already
            // wear. The count had a shelf of its own under this band until the pager
            // took the band's trailing column and left it nothing to stand on.
            <span className="grid min-w-0 grid-cols-[minmax(0,1fr)_auto] items-center gap-2">
              <span className="min-w-0 truncate">
                {compactProjectPath(root, project) || 'No workspace path'}
              </span>
              <span className="flex shrink-0 items-center gap-2">
                <span aria-hidden>·</span>
                <HeaderTally count={tally.count} unit="session" />
                <ProjectStatusCounts
                  live={tally.live}
                  awaiting={tally.awaiting}
                  unread={tally.unread}
                />
              </span>
            </span>
          }
          qualifierTitle={root}
          isOpen={isShowing}
          onToggle={() => fold(!isShowing)}
          label={`${isShowing ? 'Collapse' : 'Expand'} ${project}`}
        />
        {/* The trailing cluster holds how this group is WALKED and what it OFFERS:
            the project's pages, then its one verb. The pager rode on a shelf hung
            under this band — a second paper, a second hairline and a second sticky
            layer for one heading, 40px of the screen taken under the band for the
            whole of a project. What the group REPORTS cannot come up here with it:
            measured on a 320px screen, the count, the live pulse and the yellow verb
            take this cluster's width first and leave the project name 24px. */}
        <HeaderActions>
          {isShowing && (
            <Pager page={shownPage} pageCount={pageCount} onPage={goToPage} label={`${project} sessions`} />
          )}
          <NewSessionButton
            machine={machineLabel(conn)}
            where={project}
            busyLabel={
              creating && creating.at === `${base}\u0000${root}` ? creating.label : null
            }
            onPress={() => onNewSession(root)}
          />
        </HeaderActions>
      </SectionHeader>
      {/* The rows carry no bottom rule of their own: the trough that opens the next
          project, or the card's own bottom border, closes the group. */}
      {isShowing && rows.length > 0 && (
        <div ref={rowsRef}>
          {rows.map((session) => (
            <SessionRow
              key={session.id}
              session={session}
              draft={drafts[draftMessageKey(base, session.id)] ?? EMPTY_DRAFT_MESSAGE}
              conn={conn}
              match={matches?.get(session.id) ?? null}
              needle={needle}
              onOpen={onOpen}
              onRename={renameRow}
              onFork={forkRow}
              onDelete={deleteRow}
              onToggleStar={starRow}
              isConfirmingDelete={pendingDeleteId === session.id}
              deleteBusy={deleteBusy}
              deleteError={pendingDeleteId === session.id ? deleteError : null}
              onConfirmDelete={onConfirmDelete}
              onCancelDelete={onCancelDelete}
            />
          ))}
        </div>
      )}
    </section>
    </>
  );
});

// How long the row's disclosure takes to open or close. It is duplicated by the
// `duration-200` utilities below on purpose: the class drives the paint, this
// number only decides when the panel may leave the tree.
const STATS_MOTION_MS = 200;

const SessionRow = memo(function SessionRow({
  session,
  draft,
  conn,
  match,
  needle,
  onOpen,
  onRename,
  onFork,
  onDelete,
  onToggleStar,
  isConfirmingDelete,
  deleteBusy,
  deleteError,
  onConfirmDelete,
  onCancelDelete,
}: {
  session: Session;
  /** This device's unsent composer content for the session; EMPTY when there is none. */
  draft: DraftMessage;
  conn: GatewayConn;
  match: SessionMatch | null;
  needle: string;
  onOpen: Props['onOpen'];
  onRename: (session: Session) => void;
  /** Opens the fork question under the very cell that was pressed. */
  onFork: (session: Session, anchor: HTMLElement) => void;
  onDelete: (session: Session) => void;
  onToggleStar: (session: Session) => void;
  /** This row IS the confirm: it asks in place instead of behind a dialog. */
  isConfirmingDelete: boolean;
  deleteBusy: boolean;
  deleteError: string | null;
  onConfirmDelete: () => void;
  onCancelDelete: () => void;
}) {
  const status = statusLabel(session);
  const timestamp = session.modified_at ?? session.created_at;
  // DIRTY: this device is holding composer content nobody has sent — words, a
  // picture, a file. When the session has no title of its own, that content names
  // the row, which otherwise reads "Untitled session" with nothing on screen to
  // say why it is worth opening.
  const hasUnsent = draftMessageHasUnsent(draft);
  const title =
    session.title?.trim()
    || (hasUnsent ? firstLine(draft.text) || attachmentSummary(draft.attachments) : '')
    || 'Untitled session';
  const live = sessionIsLive(session);
  const turns = Number(session.turn_count ?? 0);
  // Turns that finished while this session was closed: the one thing a relative
  // timestamp cannot announce. The row SUBSCRIBES to the marks: the list stays
  // mounted behind the transcript, and this component is memoised over row objects
  // an unchanged poll returns identical — so without the subscription the badge of
  // the session you just read stayed on screen until something else moved.
  useReadMarks();
  const unread = unreadTurnCount(session);
  // The right chevron is a real DISCLOSURE, not decoration: it opens this
  // session's usage rollup in place. It stays a sibling of the open-session
  // button, never nested inside it, so "tell me more" cannot navigate away.
  const [statsOpen, setStatsOpen] = useState(false);
  // The rollup FETCHES on mount and aborts on unmount, so it cannot simply stay
  // mounted while closed. Dropping it on the same commit that closes the row
  // would pull the content out from under the collapse, so it outlives
  // `statsOpen` by exactly one transition and is only then let go.
  const [statsMounted, setStatsMounted] = useState(false);
  useEffect(() => {
    if (statsOpen || !statsMounted) return;
    const timer = window.setTimeout(() => setStatsMounted(false), STATS_MOTION_MS);
    return () => window.clearTimeout(timer);
  }, [statsOpen, statsMounted]);
  // Mount and open in ONE commit. The grid wrapper never leaves the tree, so
  // 0fr -> 1fr is a transition on a persistent element — no `@starting-style`,
  // which WebKit applies a frame late to freshly inserted nodes.
  const toggleStats = useCallback(() => {
    setStatsMounted(true);
    setStatsOpen((open) => !open);
  }, []);
  // The star is the GATEWAY's, and the row is holding the only copy of it there is:
  // `favorite_rank`, straight off this session. No device-side store can disagree
  // with it — which is what used to leave one screen starred and another plain.
  const isStarred = isFavorite(session);
  // Where the row GOES when this flips — the pinned band at the top of the project,
  // on page one — belongs to the group that pages it, so `ProjectGroup` owns the
  // follow. This is only the mark and the strip's verb.
  const toggleStar = useCallback(() => onToggleStar(session), [onToggleStar, session]);

  return (
    <div className="[&+&]:border-t [&+&]:border-dialog-edge">
      {/* The confirm IS the row (`ConfirmRow`, shared with a machine's `Forget`).
          Renaming and the group purge, which state a wider blast radius, still
          ask in a dialog. */}
      {isConfirmingDelete ? (
        <ConfirmRow
          question={`Delete ${title}?`}
          confirmLabel={deleteBusy ? 'Deleting...' : 'Yes, delete'}
          isBusy={deleteBusy}
          onKeep={onCancelDelete}
          onConfirm={onConfirmDelete}
        />
      ) : (
      <SwipeActions
        label={title}
        actions={[
          {
            key: 'favorite',
            label: isStarred ? 'Unstar' : 'Star',
            icon: <StarIcon filled={isStarred} className="size-4" />,
            // The one action on the strip that is not a neutral verb: it wears the
            // same brand yellow as the mark it leaves on the row.
            tone: 'accent',
            onSelect: toggleStar,
          },
          {
            key: 'rename',
            label: 'Rename',
            icon: <PencilIcon className="size-4" />,
            onSelect: () => onRename(session),
          },
          {
            key: 'fork',
            // The strip is 72px wide, so the caption is the one word; the whole
            // sentence lives in `name` for a reader who cannot see the row.
            label: 'Fork',
            name: `Fork ${title}`,
            icon: <ForkIcon className="size-4" />,
            // Forking COPIES — it takes nothing away from the row it starts on —
            // so it stays a neutral verb beside Rename, never the red one.
            onSelect: (anchor) => onFork(session, anchor),
          },
          {
            key: 'delete',
            label: 'Delete',
            icon: <TrashIcon className="size-4" />,
            tone: 'danger',
            onSelect: () => onDelete(session),
          },
        ]}
      >
      <div className="flex items-stretch">
        <button
          type="button"
          className={`group flex min-h-12 min-w-0 flex-1 items-center py-1.5 text-left transition-colors duration-150 hover:bg-hover active:bg-hover focus-visible:bg-hover focus-visible:outline-none motion-reduce:transition-none mouse:min-h-8 mouse:py-1 ${LIST_EDGE} ${LIST_EDGE_END}`}
          data-session-id={session.id}
          onClick={() => void onOpen(conn, session.id)}
        >
        {/* One row of facts, laid out twice from ONE dom order.
            A phone stacks it: what the session IS on the first line, what it has DONE
            on the second, each line's own trailing fact right-aligned against it.
            From `sm:` up there is room for the whole sentence on one line, and the
            facts stop floating: the wrapper below turns to `contents` so its children
            become grid items of the row itself, and id / turns / status / time land on
            FIXED tracks. That is the difference between a list and a phone list
            stretched to 1400px, where a title sat at x=56 and its own status badge at
            x=1325 with nothing between them to carry the eye across.
            Each fixed track is its own content's width: the status one holds its fixed
            favorite slot BEFORE the dot plus the longest label. `INPUT NEEDED` measures
            83px, so the whole cluster owns 7.25rem and neither a title nor a missing star
            can move it. The id track pays for its 8 hex characters inside 4.5rem instead
            of charging that width to the title. */}
        <span className="grid min-w-0 flex-1 grid-cols-[minmax(0,1fr)_auto_auto] items-center gap-x-3 gap-y-1 sm:grid-cols-[minmax(0,1fr)_5.5rem_4.5rem_4.5rem_7.25rem_6rem] sm:gap-y-0">
          {/* The NAME, and nothing but the name. The badges used to ride inside this
              cell, so every row started its flags at a different x — the longer the
              title, the further right its `NEW` — and a long title pushed them off
              the line entirely. They have their own column now. */}
          <span className="col-start-1 row-start-1 flex min-w-0 items-center gap-1.5 sm:col-start-auto sm:row-start-auto">
            {/* The LEAF, and the smallest name on the screen: 15 / 13 / 10 down the
                machine -> project -> session ladder. It stays the strongest thing in
                its OWN row — semibold, full ink, against the hint-grey `text-chip`
                facts beside it — so shrinking it costs the scan nothing. */}
            <span
              className={`min-w-0 truncate font-mono text-meta font-semibold ${
                session.title?.trim() ? 'text-white' : 'text-white/45'
              }`}
            >
              {title}
            </span>
          </span>
          {/* Unread and unsent-message flags share one aligned column. */}
          <span className="col-start-2 row-start-1 flex min-w-0 items-center justify-end gap-1.5 font-mono text-chip sm:col-start-auto sm:row-start-auto">
            {unread > 0 && (
              <span className="shrink-0 bg-accent px-1 font-mono text-chip font-bold uppercase tracking-[0.08em] text-accent-foreground">
                {unread > 1 ? `${unread} new` : 'new'}
              </span>
            )}
            {hasUnsent && (
              <span
                className="shrink-0 border border-warn-strong px-1 font-mono text-chip font-bold uppercase tracking-[0.08em] text-warn-strong"
                title="Unsent message waiting in this session's composer"
              >
                dirty
              </span>
            )}
          </span>
          {/* `sm:contents` is what lets one dom order be two layouts: on a phone this
              is a single line of prose under the title, and from `sm:` up it dissolves
              so that the id and the turn count become columns in their own right. */}
          {/* One rank, one ink: hierarchy is carried by SIZE (title 12px vs meta 10px),
              never by transparency — an id at 55% ink beside a `·` at 40% beside a full
              hint made one 9px line carry three different inks and none of them readable. */}
          <span className="col-start-1 row-start-2 flex min-w-0 items-center gap-x-2 font-mono text-meta text-dialog-hint sm:contents">
            <span className="truncate tabular-nums">{shortId(session.id)}</span>
            <span className="sm:hidden" aria-hidden="true">·</span>
            <span className="whitespace-nowrap font-mono text-meta text-dialog-hint tabular-nums">
              {turns} {turns === 1 ? 'turn' : 'turns'}
            </span>
          </span>
          <span
            data-session-status
            className={`col-start-3 row-start-1 inline-flex shrink-0 items-center gap-1 justify-self-end font-mono text-chip font-bold tracking-[0.08em] sm:col-start-auto sm:row-start-auto sm:justify-self-start ${statusTone(session)}`}
          >
            <span
              data-session-favorite-slot
              className="inline-flex size-3 shrink-0 items-center justify-center"
            >
              {isStarred && (
                <>
                  <StarIcon filled className="size-3" />
                  <span className="sr-only">Favorite</span>
                </>
              )}
            </span>
            <span
              data-session-status-dot
              aria-hidden="true"
              className={`size-1.5 shrink-0 ${statusDot(session)} ${live ? 'animate-pulse motion-reduce:animate-none' : ''}`}
            />
            <span>{status}</span>
          </span>
          <span
            className="col-start-2 col-end-4 row-start-2 justify-self-end whitespace-nowrap font-mono text-meta text-dialog-hint tabular-nums sm:col-start-auto sm:col-end-auto sm:row-start-auto sm:justify-self-start"
            title={formatExact(timestamp)}
          >
            {timeLabel(timestamp)}
          </span>
        </span>
        </button>
        {/* The same box, the same column and the same right edge as the `⋯` in the
            project header directly above: both promise "there is more here", so
            neither is allowed its own geometry. */}
        <HeaderActions>
          <RowDisclosure
            isOpen={statsOpen}
            label={`${statsOpen ? 'Hide' : 'Show'} details for ${title}`}
            onClick={toggleStats}
          />
        </HeaderActions>
      </div>
      </SwipeActions>
      )}
      {isConfirmingDelete && deleteError && (
        <div className="px-3 pb-2">
          <Banner kind="err">{deleteError}</Banner>
        </div>
      )}
      {/* Height eases through a 0fr -> 1fr grid track: the one pure-CSS way to
          animate to CONTENT height without measuring it, and unlike a mount it
          plays in BOTH directions. The inner clip keeps the rollup from
          spilling over the next row while the track is still closing. */}
      <div
        aria-hidden={!statsOpen}
        className={`grid transition-[grid-template-rows] duration-200 ease-[cubic-bezier(0.22,0.61,0.36,1)] motion-reduce:transition-none ${
          statsOpen ? 'grid-rows-[1fr]' : 'grid-rows-[0fr]'
        }`}
      >
        <div className="overflow-hidden">
          <div
            className={`transition-[opacity,translate] duration-200 ease-[cubic-bezier(0.22,0.61,0.36,1)] motion-reduce:transition-none ${
              statsOpen ? 'opacity-100' : '-translate-y-1 opacity-0'
            }`}
          >
            {statsMounted && <SessionStats session={session} conn={conn} />}
          </div>
        </div>
      </div>
      {match && <MatchPreview match={match} needle={needle} />}
    </div>
  );
});

// The expanded half of a session row: everything the list cannot afford to
// carry for every session at once. It is fetched HERE only when the row opens,
// and aborted if the row closes first; the gateway memoizes decoded tool tallies.
function SessionStats({ session, conn }: { session: Session; conn: GatewayConn }) {
  const [usage, setUsage] = useState<SessionUsage | null>(null);
  const [phase, setPhase] = useState<'loading' | 'ready' | 'error'>('loading');

  useEffect(() => {
    const controller = new AbortController();
    setPhase('loading');
    new GatewayClient(conn)
      .sessionUsage(session.id, controller.signal)
      .then((next) => {
        if (controller.signal.aborted) return;
        setUsage(next);
        setPhase('ready');
      })
      .catch(() => {
        if (!controller.signal.aborted) setPhase('error');
      });
    return () => controller.abort();
  }, [conn, session.id]);

  const cacheHit = usage?.cache_hit_rate;

  return (
    <div className={`border-t border-dialog-edge bg-panel-2 py-2.5 ${LIST_EDGE} ${LIST_EDGE_END}`}>
      {phase === 'loading' && (
        <p className="font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
          Reading usage…
        </p>
      )}
      {phase === 'error' && (
        <p className="font-mono text-chip uppercase tracking-[0.08em] text-warn-strong">
          Usage unavailable
        </p>
      )}
      {phase === 'ready' && !usage && (
        <p className="font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
          No turns yet
        </p>
      )}
      {phase === 'ready' && usage && (
        <>
          <dl className="grid grid-cols-4 gap-x-3 gap-y-2">
            <Stat label="Turns" value={compactCount(usage.turn_count)} />
            <Stat label="Iters" value={compactCount(usage.iteration_count)} />
            <Stat label="Tools" value={compactCount(usage.tool_call_count)} />
            <Stat label="Folds" value={compactCount(usage.fold_count)} />
            <Stat label="In" value={compactCount(usage.input_tokens)} />
            <Stat label="Out" value={compactCount(usage.output_tokens)} />
            <Stat
              label="Cache"
              value={typeof cacheHit === 'number' ? `${Math.round(cacheHit * 100)}%` : '—'}
            />
            <Stat label="Cost" value={formatUsd(usage.cost_usd)} />
          </dl>
          <dl className="mt-2.5 flex flex-wrap items-baseline gap-x-3 gap-y-1 border-t border-dialog-edge/40 pt-2">
            {/* `/usage` names the model the session actually RAN on, but only
                once a turn has finished stamping it — a live session's newest
                turn has none. The pin (list row) and the state's root model are
                the standing answers, so fall back to those instead of a dash. */}
            <Meta
              label="Model"
              value={usage.model || session.model_pref?.model || session.model || '—'}
              title={usage.provider || session.model_pref?.provider}
            />
            <Meta
              label="Active"
              value={formatDuration(usage.duration_ms)}
              title="Time spent inside turns"
            />
          </dl>
        </>
      )}
    </div>
  );
}

function Stat({ label, value }: { label: string; value: string }) {
  return (
    <div className="min-w-0">
      <dt className="truncate font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
        {label}
      </dt>
      <dd className="truncate font-mono text-chip font-bold tabular-nums text-white">{value}</dd>
    </div>
  );
}

// The grid above answers "how much"; this row answers "of what, for how long".
// It reuses the grid's dim-key/strong-value grammar so the two facts read as
// labelled data instead of one faint unlabelled sentence.
function Meta({ label, value, title }: { label: string; value: string; title?: string }) {
  return (
    <div className="flex min-w-0 items-baseline gap-1.5" title={title}>
      <dt className="shrink-0 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
        {label}
      </dt>
      <dd className="min-w-0 truncate font-mono text-meta font-bold text-white">{value}</dd>
    </div>
  );
}

function compactCount(value?: number): string {
  const n = Number(value ?? 0);
  if (!Number.isFinite(n)) return '—';
  if (n < 1_000) return String(n);
  if (n < 1_000_000) return `${(n / 1_000).toFixed(n < 10_000 ? 1 : 0)}k`;
  if (n < 1_000_000_000) return `${(n / 1_000_000).toFixed(n < 10_000_000 ? 1 : 0)}M`;
  return `${(n / 1_000_000_000).toFixed(1)}B`;
}

// Sub-cent totals must not read as "$0.00" — a session that cost something is
// never free.
function formatUsd(value?: number): string {
  const n = Number(value ?? 0);
  if (!Number.isFinite(n) || n <= 0) return '$0';
  if (n < 0.01) return '<$0.01';
  if (n < 1_000) return `$${n.toFixed(2)}`;
  return `$${Math.round(n).toLocaleString()}`;
}

function formatDuration(value?: number): string {
  const ms = Number(value ?? 0);
  if (!Number.isFinite(ms) || ms <= 0) return '0s';
  const seconds = Math.round(ms / 1_000);
  if (seconds < 60) return `${seconds}s`;
  const minutes = seconds / 60;
  if (minutes < 60) return `${Math.round(minutes)}m`;
  const hours = minutes / 60;
  return hours < 24 ? `${hours.toFixed(1)}h` : `${(hours / 24).toFixed(1)}d`;
}

// The list has nothing to paint yet. Two rules keep this honest:
//
// 1. Colour: the previous skeleton was drawn in panel tints — and `--panel2`
//    EQUALS `--surface` in the shipped themes (light: both #faf3eb), so it
//    rendered as invisible boxes. Bars use `--color-muted`, a mid grey that
//    separates from every gateway surface, and the Braille spinner says it in
//    words — the same one the transcript and the TUI use.
// 2. GEOMETRY: a placeholder that is not the exact height of the thing it
//    stands for makes the whole list jump when data lands. The skeleton mirrors
//    the compact 36px mouse project header and the session rows' density steps.
//    Each bar is centred inside an INVISIBLE glyph of the real type step, so its
//    line box — not a bare decorative bar — determines the same vertical rhythm.
// 3. ONE OWNER PER SEAM. The filter owns the line above the list, each skeleton
//    header owns the line below itself, sibling rows own their internal lines,
//    and the rows wrapper closes the group. No top border or negative margin is
//    needed, so loading and loaded states cannot stack adjacent rules.
function SkeletonBar({
  type,
  width,
  baz,
  tone,
}: {
  type: string;
  width: string;
  baz: string;
  tone: string;
}) {
  return (
    <span className={`grid ${width}`}>
      <span className={`col-start-1 row-start-1 invisible font-mono ${type}`}>&nbsp;</span>
      <span className={`col-start-1 row-start-1 self-center ${baz} ${tone}`} />
    </span>
  );
}

function NavigatorSkeleton() {
  return (
    <div role="status" aria-live="polite" aria-label="Loading sessions" className={LIST_FRAME}>
      <div className="animate-pulse motion-reduce:animate-none" aria-hidden="true">
        {SKELETON_GROUPS.map((rows, group) => (
          <div key={group}>
            {/* The list's OWN header band, so a loading screen can never stand at a
                different height from the screen it turns into. */}
            <SectionHeader rule="border-accent">
              {/* Two lines, because the header it stands in for is two: a name over
                  the path that qualifies it. A one-line skeleton grew by a line the
                  moment data landed, which is a layout jump on every cold open — so
                  the bars go through the same `name`/`qualifier` slots the real
                  header uses rather than a hand-stacked pair. */}
              <HeaderTitle
                name={<SkeletonBar type="text-title" width="w-28" baz="h-2.5" tone="bg-muted/40" />}
                qualifier={
                  <SkeletonBar type="text-chip" width="w-40" baz="h-1.5" tone="bg-muted/20" />
                }
              />
              <HeaderActions>
                <SkeletonBar type="text-chip" width="w-14" baz="h-1.5" tone="bg-muted/25" />
              </HeaderActions>
            </SectionHeader>
            {/* Mirrors `SessionRow` — the SAME grid, the same leading edge, the same
                trailing column — because a skeleton that stands anywhere else is a
                layout jump the user pays for on every cold open. It used to carry an
                `invisible` chevron and `px-3`, indenting its bars 32px in a list whose
                real rows started their titles at 8px. */}
            <div>
              {rows.map((width, row) => (
                <div
                  key={row}
                  className={`flex min-h-12 w-full items-center py-1.5 [&+&]:border-t [&+&]:border-dialog-edge mouse:min-h-8 mouse:py-1 ${LIST_EDGE}`}
                >
                  <span className="grid min-w-0 flex-1 grid-cols-[minmax(0,1fr)_auto_auto] items-center gap-x-3 gap-y-1 sm:grid-cols-[minmax(0,1fr)_5.5rem_4.5rem_4.5rem_7.25rem_6rem] sm:gap-y-0">
                    <span className="col-start-1 row-start-1 sm:col-start-auto sm:row-start-auto">
                      <SkeletonBar type="text-meta" width={width} baz="h-2.5" tone="bg-muted/30" />
                    </span>
                    <span className="col-start-1 row-start-2 flex items-center gap-x-2 sm:contents">
                      <SkeletonBar type="text-chip" width="w-14" baz="h-1.5" tone="bg-muted/20" />
                      <SkeletonBar type="text-chip" width="w-10" baz="h-1.5" tone="bg-muted/20" />
                    </span>
                    {/* The flag column a real row keeps for `NEW` / `dirty`.
                        Nothing is loading in it, but the track has to exist or the
                        columns shift the moment the rows arrive. */}
                    <span className="col-start-2 row-start-1 sm:col-start-auto sm:row-start-auto" />
                    <span className="col-start-3 row-start-1 justify-self-end sm:col-start-auto sm:row-start-auto sm:justify-self-start">
                      <SkeletonBar type="text-chip" width="w-12" baz="h-1.5" tone="bg-muted/25" />
                    </span>
                    <span className="col-start-3 row-start-2 justify-self-end sm:col-start-auto sm:row-start-auto sm:justify-self-start">
                      <SkeletonBar type="text-chip" width="w-12" baz="h-1.5" tone="bg-muted/20" />
                    </span>
                  </span>
                  <HeaderActions>
                    <span className="w-7 sm:w-8 mouse:w-6" />
                  </HeaderActions>
                </div>
              ))}
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

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

function shortId(id: string): string {
  return id.split('-')[0]?.slice(0, 8) || id.slice(0, 8);
}

function statusLabel(session: Session): string {
  // The DEMAND outranks liveness: a parked run is still live, and "LIVE" is
  // exactly what made the row look like it was getting on with it.
  if (sessionNeedsInput(session)) return 'INPUT NEEDED';
  if (sessionIsLive(session)) return 'LIVE';
  if (session.status === 'suspended') return 'WAITING';
  return 'IDLE';
}

function statusTone(session: Session): string {
  if (sessionNeedsInput(session)) return 'text-warn-strong';
  if (sessionIsLive(session)) return 'text-ok';
  if (session.status === 'suspended') return 'text-warn-strong';
  return 'text-dialog-hint';
}

function statusDot(session: Session): string {
  if (sessionNeedsInput(session)) return 'animate-pulse bg-warn-strong motion-reduce:animate-none';
  if (sessionIsLive(session)) return 'animate-pulse bg-ok motion-reduce:animate-none';
  if (session.status === 'suspended') return 'bg-warn-strong';
  return 'border border-dialog-hint';
}

function draftSearchText(draft: DraftMessage | undefined): string {
  if (!draft) return '';
  return [draft.text, ...draft.attachments.map((attachment) => attachment.filename)]
    .join(' ')
    .toLowerCase();
}

function sessionSearchText(session: Session): string {
  return [
    session.title,
    session.id,
    session.project_name,
    session.workspace?.label,
    session.workspace?.root,
    session.status,
    sessionNeedsInput(session) ? 'input needed waiting human' : '',
    sessionIsLive(session) ? 'live running' : 'idle',
  ]
    .filter(Boolean)
    .join(' ')
    .toLowerCase();
}


function dateMillis(value?: string): number {
  if (!value) return 0;
  const millis = new Date(value).getTime();
  return Number.isFinite(millis) ? millis : 0;
}


function formatExact(value?: string): string {
  const millis = dateMillis(value);
  return millis ? new Date(millis).toLocaleString() : '';
}

// Search hits stay subordinate to their session: compact transcript rows, not cards.
function MatchPreview({ match, needle }: { match: SessionMatch; needle: string }) {
  const rows =
    match.hits.length > 0
      ? match.hits
      : [
          { side: 'request' as const, snippet: match.requestSnippet?.trim() ?? '', at: null },
          { side: 'reply' as const, snippet: match.replySnippet?.trim() ?? '', at: null },
        ].filter((h) => h.snippet.length > 0);
  if (rows.length === 0) return null;
  return (
    <div className={`border-t border-dialog-edge bg-ink/30 py-1.5 ${LIST_EDGE} ${LIST_EDGE_END}`}>
      <div className="divide-y divide-dialog-edge/70">
        {rows.map((hit, index) => (
          <div
            key={`${hit.side}-${hit.at ?? index}`}
            className="grid grid-cols-[2.5rem_minmax(0,1fr)] gap-2 py-1.5"
          >
            <span
              className={`font-mono text-meta font-bold ${
                hit.side === 'request' ? 'text-you-role' : 'text-vis-role'
              }`}
            >
              {hit.side === 'request' ? 'You' : 'Vis'}
            </span>
            <p className="line-clamp-2 whitespace-pre-wrap break-words font-mono text-ui text-dialog-foreground">
              {highlightNeedle(hit.snippet, needle)}
            </p>
          </div>
        ))}
      </div>
    </div>
  );
}

// Splits `text` on the (case-insensitive) needle and wraps each hit in a
// contrast <mark> that reads on both rail colors.
function highlightNeedle(text: string, needle: string) {
  if (!needle) return text;
  const parts = text.split(new RegExp(`(${escapeRegExp(needle)})`, 'ig'));
  return parts.map((part, index) =>
    part.toLowerCase() === needle.toLowerCase() && part.length > 0 ? (
      <mark key={index} className="rounded-[2px] bg-accent/30 px-0.5 font-bold text-white">
        {part}
      </mark>
    ) : (
      <span key={index}>{part}</span>
    ),
  );
}

function escapeRegExp(value: string): string {
  return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}



// The first line of an unsent message, short enough to sit on one row. A dirty
// session has no other name, and a wall of pasted text must not become one.
function firstLine(text: string): string {
  const line = text.split('\n', 1)[0]?.trim() ?? '';
  return line.length > 80 ? `${line.slice(0, 79)}\u2026` : line;
}

// An unsent message can be nothing but a picture. Then the attachment names the
// row, because "Untitled session" says nothing about what is waiting in it.
function attachmentSummary(attachments: PendingAttachment[]): string {
  const first = attachments[0];
  if (!first) return '';
  const name = firstLine(first.filename) || first.media_type;
  return attachments.length > 1 ? `${name} +${attachments.length - 1}` : name;
}
