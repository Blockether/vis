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
  LiveCount,
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
  SectionShelf,
  Spinner,
} from '../components/ui';
import {
  Menu,
  MenuBack,
  MenuHeading,
  MenuItem,
  MenuNote,
  MENU_WIDTH,
  PANEL_SIZES,
} from '../components/Menu';
import { GatewayClient, type SessionMatch } from '../lib/gateway';
import { SessionSubscriptionHub } from '../lib/subscriptions';
import type { ForkPoint, GatewayConn, Session, SessionUsage, WorkspaceDraft } from '../lib/types';
import { homeifyPath } from '../lib/path';
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
import { DEFAULT_SESSION_PAGE_SIZE, getSessionsPerPage, subscribeSessionsPerPage } from '../lib/storage';
import { hostOf } from '../lib/endpoints';
import {
  clearMachineOutage,
  machineOutage,
  rememberMachineOutage,
} from '../lib/fleet-outage';
import {
  clearDraftMessage,
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
  groupByWorkDir,
  draftsRead,
  draftsReadKey,
  fleetError,
  isFleetLoaded,
  machineCounts,
  machineTally,
  machineKey,
  machineLabel,
  newSessionTarget,
  projectDelete,
  projectLabel,
  projectPage,
  projectTally,
  type Tally,
  reconcileMachines,
  resolveScope,
  SCOPE_ALL,
  sameOverview,
  scopedMachines,
  SEARCH_UNPLACED,
  searchFanout,
  searchOrder,
  searchTally,
  sessionIsListed,
  sessionIsLive,
  sessionNeedsInput,
  sessionMillis,
  sessionOrder,
  timeLabel,
  withSearchHits,
  isDraftWorkspace,
  projectPath,
  startAsk,
  START_IDLE,
  startFlowName,
  startFlowOn,
  startFlowOpen,
  startFlowBack,
  machineProject,
  startFlowPick,
  startFlowUnpick,
  type StartFlow,
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


function useSessionsPerPage(): number {
  const [pageSize, setPageSize] = useState(DEFAULT_SESSION_PAGE_SIZE);
  useEffect(() => {
    let active = true;
    void getSessionsPerPage().then((value) => {
      if (active) setPageSize(value);
    });
    const unsubscribe = subscribeSessionsPerPage((value) => setPageSize(value));
    return () => {
      active = false;
      unsubscribe();
    };
  }, []);
  return pageSize;
}

/**
 * How wide the folder browser is placed from. The sheet is RIGHT-aligned to the
 * control it hangs from, so the anchor math needs the width before it has ever been
 * measured — which is exactly why the number and the class that paints it live
 * together in `PANEL_SIZES` rather than being restated here.
 */
const BROWSE_WIDTH = PANEL_SIZES.browse.width;

/**
 * Where a new session begins. `trunk` is the plain session this screen always made:
 * the agent edits the project directly. The other two are DRAFTS — isolated clones
 * parked at `~/.vis/drafts/<repo>/<label>` — either forked fresh (a clone of the repo
 * as it stands, or one rewound to the last commit) or an existing draft someone
 * stashed earlier.
 */
type StartIn =
  | { kind: 'trunk' }
  | { kind: 'fork'; label: string; clean: boolean }
  | { kind: 'resume'; draft: WorkspaceDraft };

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
  // THE SCOPE IS `All` OR EXACTLY ONE MACHINE, and `SCOPE_ALL` is `All`.
  // Every machine owns a hue, a rail and a section of its own — and scoped to one
  // gateway only ONE of those hues can ever be on screen, which is a fleet the reader
  // cannot see. So the strip reads `All`, then the machines, and `All` stacks a named
  // section per machine, each under its own rail: the separate views are the point of
  // the separate colours. Scoping stays exactly as narrow as it was — one machine, its
  // counts, its verbs — for the reader who wants one computer and nothing else.
  // A FLEET OF ONE RESOLVES TO ITS MACHINE and is offered no `All`: "every machine"
  // and "this machine" would be the same list under two names.
  // The pick is only a PREFERENCE: naming a machine that is gone — or one that has
  // stopped answering under the reader's thumb — falls back to the fleet rather than
  // to an empty screen (see `resolveScope`).
  const [scopePick, setScopePick] = useState<string | null>(SCOPE_ALL);
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
  // The create in flight, and WHERE it was started: `at` is the project header whose
  // own button is saying the word, and null when the app bar's menu started it.
  // "Creating..." is a lie while a 12k-file repo is being cloned, so the busy word
  // follows the WORK: fork, enter, or plain create.
  const [creating, setCreating] = useState<{ at: string | null; label: string } | null>(null);
  const [createError, setCreateError] = useState<string | null>(null);
  const [manageProjects, setManageProjects] = useState<{
    machine: FleetMachine;
    at: { top: number; left: number };
  } | null>(null);
  // The start order — which machine, which workspace, what to call the draft — is ONE
  // value, so leaving it anywhere forgets every answer in it. The yellow `New session`
  // beside it needs none of those answers: it starts where the machine already is.
  // Portalled and viewport-anchored because the header panel clips its overflow.
  const [startFlow, setStartFlow] = useState<StartFlow>(START_IDLE);
  // The menu and its draft sub-question share one surface, so both anchor from the
  // control the order started at. Browsing is NOT that surface: it takes the screen.
  const startMenu =
    startFlow.step === 'menu' || startFlow.step === 'drafts' ? startFlow.at : null;
  const browseAt = startFlow.step === 'browse' ? startFlow.at : null;
  // Forking asks for the draft's name first: the gateway rejects a blank label, and
  // the name is what `/draft list` and every later resume will show.
  const namePrompt = startFlow.step === 'name' ? startFlow : null;
  // The control the open order hangs from — a project header's draft half, or its
  // New session button when that machine has no project yet. An element, not a ref,
  // because every header carries its own pair.
  const startAnchorEl = useRef<HTMLElement | null>(null);
  // One entry per machine+repo, kept across openings; see `forgetParkedDrafts`.
  const [draftsCache, setDraftsCache] = useState<
    Record<string, { rows: WorkspaceDraft[]; error: string | null }>
  >({});
  const [draftLabel, setDraftLabel] = useState('');
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
  // A cold fleet does not arrive at once and cannot: each gateway is its own
  // round trip, and a machine with more than one page of history patches its rows
  // again per page (`listSessions`' progressive `onPage`). Every one of those
  // patches inserts rows ABOVE the sections below it — with two gateways paired,
  // the first machine's second page pushes the whole second machine down under a
  // reader who is looking at it, which is the list "jumping by itself" while its
  // projects load in. A failure does the same in reverse: a machine that stops
  // answering is drained out of `All`, and its section leaves a hole.
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

  // The star belongs to the GATEWAY, and this is the one place that asks it to move.
  // The row is repainted first so the mark lands in the SAME commit as the tap, the
  // PATCH follows, and the row the gateway echoes replaces the guess. A star this
  // device could not deliver goes back: a screen must never keep saying something
  // the machine never heard.
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
      // A star DOES move its row (`sessionOrder` bands starred rows first) and the
      // reader asked for that, so this is a safe moment to take the order.
      adoptRef.current();
      void api
        .setSessionFavorite(session.id, starring)
        .then((row) => patchMachine(key, withRank(favoriteRank(row))))
        .catch(() => patchMachine(key, withRank(before)));
    },
    [patchMachine],
  );
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
          // The parked rows arrive BESIDE the window (`parkedSessions`), so a demand
          // that appeared or was answered is news even when the window's own rows are
          // word for word what is already on screen.
          const parked = reconcileSessions(held?.awaiting ?? null, api.parkedSessions());
          if (
            held &&
            held.error === null &&
            held.answered &&
            merged === held.sessions &&
            parked === held.awaiting
          )
            return;
          patchMachine(key, (machine) => ({
            ...machine,
            sessions: merged,
            awaiting: parked,
            error: null,
            answered: true,
            isRemembered: false,
          }));
        };
        // The header row's numbers, in ONE request, beside the window — never a
        // tally of the rows that happen to have landed. It rides the same load so
        // a poll refreshes counts and rows together; an overview that fails is not
        // a failed load (the list is still the answer), it just leaves the last
        // numbers standing.
        //
        // AN OVERVIEW THAT SAYS NOTHING NEW IS NOT NEWS (the same rule `settle`
        // keeps): patching regardless handed the list a fresh fleet array every
        // poll, and with it every memo built from it, for numbers that had not
        // moved.
        void api
          .projectsOverview(signal)
          .then((overview) => {
            if (signal?.aborted) return;
            const held = machinesRef.current.find(
              (machine) => machineKey(machine.conn) === key,
            );
            if (held?.overview && sameOverview(held.overview, overview)) return;
            patchMachine(key, (machine) => ({ ...machine, overview }));
          })
          .catch(() => {});
        // Paint the first page the moment it lands instead of waiting for the whole
        // fleet to drain. Only ever called on a cold load (see `listSessions`).
        const next = await api.listSessions(signal, (partial) => {
          if (signal?.aborted) return;
          alive();
          settle(partial);
        });
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
  // (a cached fleet already is), and the ten-second poll below stays the business of the
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
  // every 10s and re-ran the filter and the sort of the whole fleet — under the
  // composer the reader was typing in. Becoming visible re-runs the effect, whose
  // first act is a full load, so the rows are fresh the moment they are back on
  // the glass.
  useEffect(() => {
    if (!isVisible) return;
    const controller = new AbortController();
    const refreshLiveStates = () => {
      if (document.visibilityState === 'visible') void load(controller.signal, true);
    };

    void load(controller.signal);
    // 10s: slow enough to stay cheap, fast enough that a phone picked up mid-turn
    // shows the truth. Cheap on BOTH ends — an unchanged fleet comes back as a 304
    // with no body (see `GatewayClient.listSessions`), and `load(_, true)` drops a
    // tick that fires while the previous one is still in flight instead of queueing
    // it. Hidden tabs poll not at all.
    const timer = window.setInterval(refreshLiveStates, 10_000);
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
      const sessions = withSearchHits(machine.sessions ?? [], hits).filter((session) => {
        const listed = sessionIsListed(session, {
          hasDraftMessage: draftMessageHasUnsent(draftFor(session)),
          isFavorite: isFavorite(session),
        });
        if (!listed) return false;
        return (
          !needle || titleHit(session) || metaHit(session) || matches?.has(session.id) === true
        );
      });
      // A query RE-ORDERS what it matched and the GATEWAY decides how: the
      // search answer arrives running-sessions-first, then FRESHEST first — the
      // very order the gateway lists sessions in — so a query narrows the list
      // instead of reshuffling it, and the dates down the rows only ever fall.
      // Painting the band instead (`SessionMatch.rank`: title, then the user's
      // words, then the assistant's) buried this morning's session under every
      // year-old title holding the word. Rows the gateway did not place — an
      // unsent draft in this device's composer, local metadata — fall in behind.
      const ranked = needle
        ? searchOrder(sessions, (session) => searchPlaces?.get(session.id) ?? SEARCH_UNPLACED)
        : sessions;
      // Starred rows first, then unsent work, and with them the project group that
      // owns them: see `sessionOrder`.
      return {
        machine,
        sessions: sessionOrder(ranked, {
          favoriteRank,
          hasDraftMessage: (session) => draftMessageHasUnsent(draftFor(session)),
        }),
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
      filtered.map((entry) => ({
        machine: entry.machine,
        ...holdOrder(
          epoch,
          entry.sessions,
          (session) => ({
            id: session.id,
            millis: sessionMillis(session),
          }),
          mintedSet,
        ),
      })),
    [epoch, filtered, mintedSet],
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
  // `All`: the list is the whole fleet, so every section has to NAME the machine it
  // belongs to. Scoped, the strip above has already named it and the band is noise.
  const isFleetView = scope === SCOPE_ALL;
  // What the `All` tile has to report: something happened on a machine that is not
  // the one you were reading. The exact count belongs to the rows that own it.
  // A machine that is not answering is not in `All` at all, so its stale badge is not
  // news the `All` tile can hand back.
  const fleetUnread = machines.some(
    (machine) =>
      !machine.error && (tallies.get(machineKey(machine.conn))?.unread ?? 0) > 0,
  );

  // One hue per paired machine, assigned from the machine's own key, so a rail
  // keeps its colour across reloads and reorderings and two machines side by side
  // never share one. Colour is what the eye reads before the name, and the same
  // hue rides the scope chip above the list and the rail down its left.
  const machineColors = useMemo(
    () => assignMachineColors(machines.map((machine) => machineKey(machine.conn))),
    [machines],
  );

  const selectScope = useCallback((next: string | null) => {
    setScopePick(next);
    // Drafts are repo-scoped ON a machine, and the scope is what decides which
    // machine the next session lands on: the open order asks a question that just
    // changed underneath it, so it ends rather than answering the old one.
    setStartFlow(START_IDLE);
  }, []);

  // ONE sheet, opened from wherever the machine is named: the row above the card when
  // the list is scoped, and that machine's own band in the fleet view. It is anchored
  // on the button that was pressed, so the anchor travels with the verb.
  const openManageProjects = useCallback((machine: FleetMachine, anchor: HTMLElement) => {
    const at = menuPosition(anchor.getBoundingClientRect(), BROWSE_WIDTH);
    if (!at) return;
    setManageProjects({ machine, at });
  }, []);

  /**
   * Drafts are REPO-scoped, and the gateway only lists them through a session that
   * lives in that repo. The picker therefore reads them off the most recent session
   * that has a workspace, and the menu NAMES that repo — a fleet spanning several
   * projects must not be told these drafts belong to whatever it creates next.
   */
  // Which machine the start order is about while the bar speaks for the whole fleet: the
  // scoped machine, or the only one paired. `null` means the app must ASK before it
  // can create anything.
  const scopeTarget = newSessionTarget(machines, scope);
  // Several machines: the menu asks WHICH first, and that answer — not a session on
  // trunk — is what the workspace question below is then asked about.
  const ask = startAsk(machines, scopeTarget, startFlowOn(startFlow));
  const target = ask.on;
  // That answer was given INSIDE this menu — not by the scope, and not by the project
  // header the order started on — so it is the one answer the menu can take back.
  const pickedHere = startFlow.step === 'menu' && startFlow.on !== null;
  const targetMachine = ask.machine;
  // Which project header opened the draft picker, so the list it reads is that
  // project's own and not merely the machine's most recent one.
  const [draftRoot, setDraftRoot] = useState<string | null>(null);
  // Parked drafts belong to a PROJECT, and the split button that asks for them sits on
  // that project's header — so the probe is a session in the very root that was tapped.
  // Falling back to any session of the machine keeps every other caller unchanged.
  const draftProbe = useMemo(
    () =>
      targetMachine?.sessions?.find(
        (session) => draftRoot !== null && projectPath(session) === draftRoot,
      ) ??
      targetMachine?.sessions?.find((session) => projectPath(session)) ??
      null,
    [targetMachine, draftRoot],
  );
  const draftRepo = draftProbe ? projectLabel([draftProbe]) : '';
  // Where that machine is working RIGHT NOW: the root of its most recent session.
  // "New session" needs no question because of this — the machine has been somewhere.
  const project = useMemo(() => machineProject(targetMachine), [targetMachine]);
  // Every root that machine already runs sessions in, so the browse sheet can badge
  // a folder the user has been in instead of making them recognise the path.
  const knownRoots = useMemo(() => {
    const roots = new Set<string>();
    for (const session of targetMachine?.sessions ?? []) {
      const root = projectPath(session);
      if (root) roots.add(root);
    }
    return roots;
  }, [targetMachine]);

  // The parked drafts are read for the question that OFFERS them, and for nothing
  // else: while the menu is still asking WHICH machine there is no repo to read them
  // from, and it must not wait on a list it does not show.
  const isWorkspaceAsk = startMenu !== null && target !== null;
  // WHAT the picker reads, as a value with a string identity. The effect below
  // depends on that identity and reaches the value through a ref, so the objects
  // changing underneath it — a background poll replacing the machine and its
  // sessions, `resize` re-anchoring the menu — cannot abort a request in flight.
  const draftsSource = draftsRead(targetMachine, draftProbe);
  const draftsSourceKey = draftsReadKey(draftsSource);
  const draftsSourceRef = useRef(draftsSource);
  useEffect(() => {
    draftsSourceRef.current = draftsSource;
  }, [draftsSource]);
  // The answer for that key, KEPT. Creating a session mints a row in the very repo
  // the picker reads, so the menu reopens on a list this screen already holds:
  // going back to the gateway for an answer that cannot have changed is exactly the
  // "Reading drafts..." jump on every single New session. Presence of the entry —
  // not `null` rows — is what "already read" means, so a repo with nothing parked
  // is an answer too and is not re-asked either.
  const draftsEntry = draftsCache[draftsSourceKey] ?? null;
  const drafts = draftsEntry?.rows ?? null;
  const draftsError = draftsEntry?.error ?? null;
  const isDraftsRead = draftsEntry !== null;

  // A fork or a resume changed THAT repo's parked list. Forget that one key; every
  // other machine and repo keeps the list it already paid for.
  const forgetParkedDrafts = useCallback((key: string) => {
    setDraftsCache((cache) => {
      if (!(key in cache)) return cache;
      const next = { ...cache };
      delete next[key];
      return next;
    });
  }, []);

  /**
   * Open — or RE-ANCHOR — the start order on the control it came from. Re-anchoring
   * keeps every answer already in it: a resize is not an answer, and on a phone the
   * on-screen keyboard fires one in the very tap that opens the menu.
   */
  const openStartMenu = useCallback(() => {
    const at = menuPosition(startAnchorEl.current?.getBoundingClientRect(), MENU_WIDTH);
    setStartFlow((flow) => startFlowOpen(flow, at));
  }, []);

  /**
   * The draft half of a project header's split button: one tap lands on the draft
   * question with machine AND project already answered. It used to be a row inside a
   * machine-level menu, two headers above the project it actually forks.
   */
  const openDraftsAt = useCallback((anchor: HTMLElement, on: GatewayConn, root: string) => {
    startAnchorEl.current = anchor;
    const at = menuPosition(anchor.getBoundingClientRect(), MENU_WIDTH);
    if (!at) return;
    setDraftRoot(root);
    setStartFlow({ step: 'drafts', at, on });
  }, []);

  // Leaving the order — tap outside, Escape, Cancel, or a session actually created
  // — forgets every answer in it, INCLUDING which machine. That machine used to
  // outlive the dialog it was picked for: the next "New session" tap found a target
  // already set and created a session on it without asking a single question.
  const leaveStart = useCallback((restoreFocus = false) => {
    setStartFlow(START_IDLE);
    if (restoreFocus) startAnchorEl.current?.focus();
  }, []);

  // Read the parked drafts ONCE per machine+repo, and again only after a fork or a
  // resume dropped that key. A failure is reported IN the menu: the three fixed
  // choices above it still work without it.
  //
  // Deps are the OPEN flag, whether that key is already answered, and the key —
  // never the menu position or the machine object: on a phone the on-screen keyboard
  // fires `resize` in the very tap that opens the menu, and the fleet poll replaces
  // the machine every few seconds. Depending on either aborted this request on the
  // frame it started, over and over, and the menu never left "Reading drafts...".
  useEffect(() => {
    if (!isWorkspaceAsk || isDraftsRead) return;
    const source = draftsSourceRef.current;
    // That machine's first session list has not landed: keep reading, do not
    // answer "nothing parked" on behalf of a project we have not seen yet.
    if (source.kind === 'wait') return;
    const key = draftsSourceKey;
    const remember = (rows: WorkspaceDraft[], error: string | null) =>
      setDraftsCache((cache) => ({ ...cache, [key]: { rows, error } }));
    if (source.kind === 'none') {
      remember([], null);
      return;
    }
    const controller = new AbortController();
    void clientFor(source.conn)
      .drafts(source.sid, controller.signal)
      .then((rows) => remember(rows, null))
      .catch((cause) => {
        if (controller.signal.aborted) return;
        remember([], (cause as Error).message);
      });
    return () => controller.abort();
  }, [isWorkspaceAsk, isDraftsRead, draftsSourceKey]);

  // An anchored popover whose anchor moved is a lie, so a resize RE-ANCHORS it to
  // the live caret; only a caret that has left the document closes it. Closing on
  // resize is what made this menu look dead on a phone: the on-screen
  // keyboard hiding — one tap after the filter, in the very tap that opens this
  // menu — fires `resize`, and the menu died on the frame it was born. Escape
  // closes it and hands focus back to the caret it came from.
  useEffect(() => {
    if (!startMenu) return;
    const onKey = (event: KeyboardEvent) => {
      if (event.key === 'Escape') leaveStart(true);
    };
    window.addEventListener('keydown', onKey);
    window.addEventListener('resize', openStartMenu);
    return () => {
      window.removeEventListener('keydown', onKey);
      window.removeEventListener('resize', openStartMenu);
    };
  }, [startMenu, leaveStart, openStartMenu]);

  /**
   * Create the session, then put it where the user asked. The workspace move is a
   * SECOND call by construction (the gateway forks through the session that will own
   * the draft), so a failed fork must not leave a session sitting on trunk — the one
   * place the user said not to work. It has no turns yet, so it is taken back out.
   */
  async function createSession(
    startIn: StartIn = { kind: 'trunk' },
    on: GatewayConn | null = target,
    root?: string,
  ) {
    // Several machines in scope: the app cannot guess which one should run this
    // session, so it asks instead of creating one somewhere arbitrary.
    if (!on) {
      openStartMenu();
      return;
    }
    setCreating({
      // WHICH button was pressed, so only that project header goes busy.
      at: root ? `${clientFor(on).base}\u0000${root}` : null,
      label:
        startIn.kind === 'fork' ? 'Forking...' : startIn.kind === 'resume' ? 'Entering...' : 'Creating...',
    });
    setCreateError(null);
    leaveStart();
    try {
      const api = clientFor(on);
      // The machine's CURRENT project, or the one just browsed to. Absent only for a
      // machine that has never run a session: the gateway then picks its own default.
      const session = await api.createSession(root ? { root } : {});
      if (startIn.kind !== 'trunk') {
        try {
          if (startIn.kind === 'fork')
            await api.createDraft(session.id, startIn.label, startIn.clean);
          else await api.resumeDraft(session.id, startIn.draft.workspace_id);
        } catch (cause) {
          await api.deleteSession(session.id).catch(() => {});
          throw cause;
        }
        // The repo's draft list just changed; re-read it next time the menu opens.
        forgetParkedDrafts(draftsSourceKey);
      }
      // Regression, user report (paraphrased: creating a new session from the app
      // took several seconds): the fleet re-read used to stand between the tap and
      // the session. It is a full walk of every 100-row window on every machine —
      // 11 serial round trips and ~728KB on a 1100-session gateway — and it repaints
      // a list the user is leaving. Open FIRST, then refresh in the background: this
      // screen stays mounted behind the session, so the rows are already reconciled
      // by the time anyone comes back to them.
      // The row the reader just asked for is theirs, so it joins the list without
      // waiting behind the pill (see `mintedSet`).
      if (session.id) setMinted((was) => [session.id, ...was].slice(0, MINTED_KEEP));
      if (session.id) await onOpen(on, session.id, true);
      void load();
    } catch (cause) {
      setCreateError((cause as Error).message);
    } finally {
      setCreating(null);
    }
  }


  function askDraftName(clean: boolean) {
    if (!target) return;
    setDraftLabel('');
    // The machine travels WITH the order into the name dialog, instead of waiting
    // behind it in a state of its own that dismissing the dialog would leave set.
    setStartFlow(startFlowName(target, clean));
  }

  function commitDraftName() {
    const label = draftLabel.trim();
    if (startFlow.step !== 'name' || !label) return;
    const { on, clean } = startFlow;
    setStartFlow(START_IDLE);
    void createSession({ kind: 'fork', label, clean }, on);
  }

  const startRename = useCallback((session: Session, conn: GatewayConn) => {
    setRowAction({ mode: 'rename', session, conn });
    setRenameDraft(session.title?.trim() ?? '');
    setActionError(null);
  }, []);

  /**
   * The slide's `Fork` verb: hang the fork question under the cell the thumb
   * touched, exactly like the project header's draft half — the row's own strip
   * is gone by the time the panel paints, so the anchor comes with the verb.
   */
  const startFork = useCallback((session: Session, conn: GatewayConn, anchor: HTMLElement) => {
    startAnchorEl.current = anchor;
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
    if (restoreFocus) startAnchorEl.current?.focus();
  }, []);

  // The turns are read ONCE per opening, keyed by the ROW — never by the flow
  // object or the machine, both of which the 5.5s fleet poll replaces under an
  // open panel (the drafts menu learned this the hard way and never left
  // "Reading drafts..."), so the source travels in a ref like that one's does.
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
  const startProjectDelete = useCallback(
    (project: string, sessions: Session[], conn: GatewayConn) => {
      setRowAction({ mode: 'project', project, sessions, conn });
      setActionError(null);
      setActionProgress(null);
    },
    [],
  );

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
    const forgetDrafts = (ids: string[]) => {
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
        forgetDrafts([rowAction.session.id]);
        forgetRows([rowAction.session.id]);
      } else {
        const plan = projectDelete(rowAction.sessions);
        // One recursive request when a project row owns the whole group: the gateway
        // deletes the members it knows about, which is more than this list paints.
        if (plan.kind === 'project') {
          const deleted = await api.deleteProject(plan.projectId);
          forgetDrafts(deleted);
          forgetRows(deleted);
        } else {
          // No project row to hand the group to, so the fan-out IS the delete. It
          // keeps going past a failure and says what survived, instead of stopping
          // half way with nothing said.
          const gone: string[] = [];
          let failed = 0;
          setActionProgress({ done: 0, total: plan.sessionIds.length });
          for (const sid of plan.sessionIds) {
            try {
              await api.deleteSession(sid);
              gone.push(sid);
            } catch {
              failed += 1;
            }
            setActionProgress({ done: gone.length + failed, total: plan.sessionIds.length });
          }
          forgetDrafts(gone);
          // A partial fan-out is exactly known too: the ids that died leave, the ones
          // that refused keep their rows, and the note says how many refused.
          forgetRows(gone);
          if (failed > 0) {
            setActionError(`${failed} of ${plan.sessionIds.length} sessions could not be deleted.`);
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
  const sections = useMemo(
    () =>
      heldRows.map((entry) => ({
        machine: entry.machine,
        // Group identity and every create action keep the gateway's canonical path.
        // Home-shortening is paint only; feeding `~/vis` back as an API root is how an
        // older gateway produced the impossible `/…/vis/~/vis` directory.
        groups: groupByWorkDir(entry.rows),
      })),
    [heldRows],
  );

  // The projects the "remove sessions" step offers are the ones this machine is
  // SHOWING, read from the same grouping the list renders — a row that promises to
  // remove 975 transcripts under a header reading 712 is a row nobody should press.
  const managedProjects = useCallback(
    (machine: FleetMachine): ManagedProject[] =>
      groupByWorkDir(machine.sessions ?? []).map(([, sessions]) => ({
        name: projectLabel(sessions),
        root: projectRoot(sessions),
        count: sessions.length,
        live: sessions.filter(sessionIsLive).length,
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
          raised tile inside it. There is no "All": a scope is one machine, always.

          THE SWITCH IS ALWAYS THERE, EVEN FOR A FLEET OF ONE. It used to disappear
          below two machines, on the reasoning that a choice of one is not a choice —
          but the tile is not only a choice, it is the LABEL of everything under it:
          which computer these projects and sessions are on, in the machine's own hue,
          in the same place whatever the fleet size. Hiding it made a solo user's list
          belong to nobody and made pairing a second machine rearrange the screen. One
          machine is one tab, already pressed, and the second one lands beside it.

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
              {/* `All` LEADS THE STRIP, above a fleet only: it is where the machines
                  are actually distinguishable — one section, one hue and one rail per
                  computer — and a fleet of one would only be offering the same list
                  under a second name. */}
              {machines.length > 1 && (
                <MachineTab
                  isOn={isFleetView}
                  hasUnread={fleetUnread}
                  onClick={() => selectScope(SCOPE_ALL)}
                >
                  All
                </MachineTab>
              )}
              {machines.map((machine) => {
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
            {sections.map(({ machine, groups }, sectionIndex) => {
              const key = machineKey(machine.conn);
              const color = machineColor(machineColors, key);
              const address = hostOf(machine.conn.url);
              // What this section is SHOWING. Unfiltered that is the GATEWAY's own
              // total (`/v1/projects/overview`), not a tally of the rows this
              // device has paged in — the list is a window, so counting it read
              // low and moved as pages landed. A search or filter narrows the list
              // on this device, and then the honest number is what is on screen.
              const band = searching
                ? machineTally(null, groups)
                : machineTally(machine.overview, groups);
              const shown = band.count;
              const shownLive = band.live;
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
                  {/* IN THE FLEET VIEW A SECTION NAMES ITS MACHINE, and only there.
                      Scoped, the strip directly above the card has just said the name
                      and a band repeating it is the second one this list was reported
                      for; with every machine on screen at once, a rail with no name at
                      the top of it is a colour the reader cannot resolve. It is the
                      same BAND as the project headers below it — the rule under it is
                      the machine's own hue, so where one computer ends is a colour
                      change carrying a name. */}
                  {isFleetView && (
                    <SectionHeader rule={color.rail}>
                      <HeaderTitle
                        mark={<MachineMark color={color} size="banner" />}
                        name={machineLabel(machine.conn)}
                        qualifier={address === machineLabel(machine.conn) ? undefined : address}
                        qualifierTitle={machine.conn.url}
                      />
                      <HeaderActions>
                        {/* Every machine in this list is answering — `All` is the
                            machines that answered (see `scopedMachines`) — so a band
                            always has a count to carry. */}
                        <HeaderMeta>
                          <HeaderTally count={shown} unit="session" />
                          <LiveCount count={shownLive} />
                        </HeaderMeta>
                        {/* The machine's own control, on the machine's own band — the
                            row above the card cannot carry it here, because in this view
                            it speaks for no single machine. */}
                        <MachineProjectsButton
                          machine={machineLabel(machine.conn)}
                          onPress={(anchor) => openManageProjects(machine, anchor)}
                        />
                      </HeaderActions>
                    </SectionHeader>
                  )}
                  {groups.length === 0
                    ? (
                        <div className="flex flex-wrap items-center gap-3 px-3 py-3 sm:px-4">
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
                          {/* A MARK CANNOT TEACH ITSELF ON AN EMPTY MACHINE: the band
                              above carries the folder, and with no project row under it
                              there is nothing on screen that says what the folder opens.
                              A machine that is up and genuinely empty therefore gets the
                              word — the same control, the same sheet, spelled out once,
                              where it is the only thing to press. A search that matched
                              nothing is not that seam: the projects are there, the query
                              simply missed them. */}
                          {!searching && machine.sessions !== null && (
                            <MachineProjectsButton
                              machine={machineLabel(machine.conn)}
                              face="word"
                              onPress={(anchor) => openManageProjects(machine, anchor)}
                            />
                          )}
                        </div>
                      )
                    : groups.map(([groupRoot, projectSessions], groupIndex) => (
                        // Two projects used to be separated by the SAME hairline that
                        // separates two sessions of one project. Every group after the
                        // first opens on 8px of the machine's own paper instead.
                        <Fragment key={`${key}\u0000${groupRoot}`}>
                        {groupIndex > 0 && <SectionGap />}
                        <ProjectGroup
                          project={projectLabel(projectSessions)}
                          sessions={projectSessions}
                          tally={
                            searching
                              ? projectTally(null, groupRoot, projectSessions)
                              : projectTally(machine.overview, groupRoot, projectSessions)
                          }
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
                          onNewSession={(root) => void createSession({ kind: 'trunk' }, machine.conn, root)}
                          creating={creating}
                          // A draft is not a preference: every project header offers
                          // the private copy beside its own "New session".
                          onNewDraft={(anchor, root) => openDraftsAt(anchor, machine.conn, root)}
                          pageSize={pageSize}
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

      {startMenu && (
        <Menu
          label={target ? 'Start the new session in' : 'Create the new session on'}
          at={startMenu}
          onDismiss={() => leaveStart(true)}
        >
          {target ? (
            <>
              {/* WHICH machine was answered inside this very menu, so that answer can be
                  taken back without leaving the order. A draft question opened from a
                  project header was never asked it: it gets a heading, never a Back to a
                  question nobody answered. */}
              {pickedHere ? (
                <MenuBack
                  label="Back to which machine runs this session"
                  onBack={() => setStartFlow(startFlowUnpick)}
                >
                  Start the session in · {machineLabel(target)}
                </MenuBack>
              ) : (
                <MenuHeading>
                  Start the session in
                  {machines.length > 1 ? ` · ${machineLabel(target)}` : ''}
                </MenuHeading>
              )}
              <MenuItem
                title="The project itself"
                hint="Edits land straight in the repo — no isolated copy."
                badge="Default"
                onSelect={() => void createSession({ kind: 'trunk' }, target, project?.path)}
              />
              <MenuItem
                title="A new draft, with my uncommitted changes"
                hint="A private copy of this project exactly as it is now — your uncommitted changes come with it. The real project stays untouched."
                onSelect={() => askDraftName(false)}
              />
              <MenuItem
                title="A new draft, without my uncommitted changes"
                hint="A private copy of this project as of your last commit. Your uncommitted work stays here, in the real project, untouched."
                onSelect={() => askDraftName(true)}
              />
              {/* The SECOND band of the same menu, so it is the quiet one: a treatment
                  that shouts once is a barcode when it is charged twice. */}
              <MenuHeading tone="quiet">
                Or a draft you parked{draftRepo ? ` · ${draftRepo}` : ''}
              </MenuHeading>
              {drafts === null ? (
                <MenuNote>
                  <Spinner tone="accent" />
                  Reading drafts...
                </MenuNote>
              ) : drafts.length === 0 ? (
                <MenuNote>{draftsError ?? 'No drafts parked in this project yet.'}</MenuNote>
              ) : (
                drafts.map((draft) => (
                  <MenuItem
                    key={draft.workspace_id}
                    title={draft.label?.trim() || shortId(draft.workspace_id)}
                    hint={draftHint(draft)}
                    badge={draft.is_current ? 'in use' : undefined}
                    onSelect={() => void createSession({ kind: 'resume', draft })}
                  />
                ))
              )}
            </>
          ) : (
            /* No machine is in scope, so the session has no home yet. The draft
               question comes AFTER this one — a workspace only exists on a
               machine — so this menu asks the one question that has to be first. */
            <>
              <MenuHeading>Create the session on</MenuHeading>
              {ask.choices.length === 0 ? (
                <MenuNote>No paired machine is answering right now.</MenuNote>
              ) : (
                ask.choices.map((machine) => {
                  const tally = tallies.get(machineKey(machine.conn));
                  const count = tally?.sessions ?? 0;
                  return (
                    <MenuItem
                      key={machineKey(machine.conn)}
                      title={machineLabel(machine.conn)}
                      hint={`${count} ${count === 1 ? 'session' : 'sessions'} · ${hostOf(machine.conn.url)}`}
                      badge={tally?.live ? `${tally.live} live` : undefined}
                      onSelect={() => {
                        // An ANSWER, never the whole order: the workspace question
                        // comes next, and the drafts it offers are parked on THIS
                        // machine — another key, read on the very next frame.
                        setStartFlow((flow) => startFlowPick(flow, machine.conn));
                      }}
                    />
                  );
                })
              )}
            </>
          )}
        </Menu>
      )}

      {/* THE FORK QUESTION, under the row's own slide.

          It rises from the same anchored panel the drafts question uses, because
          it asks the same shape of thing: one default at the top, then the list
          the reader may cut at instead. Forking is never destructive — the row
          it was started from is untouched — so there is no confirm in front of
          it, only the choice of WHERE the copy stops. */}
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

      {/* The folder browser the start flow falls through to when a machine has no
          project yet. `manageProjects` below is the same sheet, reached deliberately
          from the projects mark on the row above the list. */}
      {target && browseAt && (
        <ManageProjectsSheet
          label={machineLabel(target)}
          at={browseAt}
          client={clientFor(target)}
          startAt={project?.path ?? null}
          knownRoots={knownRoots}
          projects={targetMachine ? managedProjects(targetMachine) : []}
          onCancel={() => setStartFlow(startFlowBack)}
          onChoose={(root) => void createSession({ kind: 'trunk' }, target, root)}
          onRemove={(entry) => {
            const machine = targetMachine;
            if (!machine) return;
            leaveStart();
            startProjectDelete(
              entry.name,
              (machine.sessions ?? []).filter((session) => projectPath(session) === entry.root),
              target,
            );
          }}
        />
      )}

      {manageProjects && (
        <ManageProjectsSheet
          label={machineLabel(manageProjects.machine.conn)}
          at={manageProjects.at}
          client={clientFor(manageProjects.machine.conn)}
          startAt={machineProject(manageProjects.machine)?.path ?? null}
          knownRoots={new Set(
            (manageProjects.machine.sessions ?? [])
              .map(projectPath)
              .filter((path): path is string => !!path),
          )}
          projects={managedProjects(manageProjects.machine)}
          onCancel={() => setManageProjects(null)}
          onChoose={(_root: string) => setManageProjects(null)}
          onRemove={(entry) => {
            const conn = manageProjects.machine.conn;
            const sessions = (manageProjects.machine.sessions ?? []).filter(
              (session) => projectPath(session) === entry.root,
            );
            setManageProjects(null);
            startProjectDelete(entry.name, sessions, conn);
          }}
        />
      )}

      {namePrompt && (
        <Modal onDismiss={() => leaveStart()}>
            <DialogFrame
              title={namePrompt.clean ? 'Name the clean draft' : 'Name the draft'}
              onClose={() => leaveStart()}
            >
              <div className="space-y-3 p-4">
                <p className="font-mono text-meta text-dialog-hint">
                  {namePrompt.clean
                    ? 'A private copy of this project as of your last commit — your uncommitted changes stay here and are not copied in. Applying it later is what moves the work back.'
                    : 'A private copy of this project exactly as it is now, uncommitted changes included. Applying it later is what moves the work back.'}
                </p>
                <label className="block">
                  <span className="mb-1 block font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
                    Draft name
                  </span>
                  <Input
                    autoFocus
                    value={draftLabel}
                    maxLength={80}
                    placeholder="wire-rework"
                    onChange={(event) => setDraftLabel(event.target.value)}
                    onKeyDown={(event) => {
                      if (event.key === 'Enter') commitDraftName();
                    }}
                  />
                </label>
                <div className="flex justify-end gap-2">
                  <Button variant="secondary" onClick={() => leaveStart()}>
                    Cancel
                  </Button>
                  <Button
                    variant="primary"
                    disabled={!draftLabel.trim()}
                    onClick={commitDraftName}
                  >
                    Create
                  </Button>
                </div>
              </div>
            </DialogFrame>
        </Modal>
      )}
    </section>
  );
}

/** Rename one row, delete one row, or delete a whole group on ONE machine. */
type RowAction =
  | { mode: 'rename'; session: Session; conn: GatewayConn }
  | { mode: 'delete'; session: Session; conn: GatewayConn }
  | { mode: 'project'; project: string; sessions: Session[]; conn: GatewayConn };

/**
 * What the confirm says.
 *
 * A group delete states the FULL blast radius: the count is every session in the
 * group, including the ones `sessionIsListed` hides, and it names the machine —
 * the same repo on two machines is two groups. It also never claims to delete a
 * project when the group is only a shared label.
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
  const plan = projectDelete(action.sessions);
  const count = plan.sessionIds.length;
  const sessions = `${count} ${count === 1 ? 'session' : 'sessions'}`;
  return {
    title: 'Purge sessions',
    subject: `${action.project} · ${machine}`,
    body:
      plan.kind === 'project'
        ? `Purge all ${sessions} in this project, with every transcript, from ${machine}? This cannot be undone.`
        : `Purge all ${sessions} in this group, with every transcript, from ${machine}? They share a workspace but no saved project, so nothing else is removed. This cannot be undone.`,
    live: action.sessions.filter(sessionIsLive).length,
  };
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
  onNewDraft,
  pageSize,
  isTop,
}: {
  project: string;
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
  /** Opens the private-copy question for this project, anchored on the button. */
  onNewDraft?: (anchor: HTMLElement, root: string) => void;
  pageSize: number;
  /**
   * This is the project the machine's own order put ON TOP, and the one project
   * that opens without being asked. Everything below it starts folded.
   */
  isTop: boolean;
}) {
  const root = projectRoot(sessions);
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

  // A project is WALKED, page by page, and every page is cut from the rows this
  // screen already paints: `projectPage` owns that arithmetic and the reason it
  // is not the gateway's.
  const [page, setPage] = useState(1);
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
  const {
    page: shownPage,
    pageCount,
    rows,
  } = useMemo(() => projectPage(sessions, page, pageSize), [sessions, page, pageSize]);
  useEffect(() => {
    // The fleet moved under the pager (a deletion, a filter, a smaller step): the
    // page that no longer exists becomes the first one rather than the last one a
    // reader never asked for.
    if (page > pageCount) setPage(1);
  }, [page, pageCount]);
  // A star PINS its row to the top of the project, and the top of the project is
  // PAGE ONE — so a row starred from any other page LEFT the page under the thumb
  // that starred it. Nothing was broken about the mark: the row carrying it was two
  // pages away, which is why it only ever turned up after the screen was left and
  // re-entered on page one.
  // Regression, user report: after starring, no star appeared on the session row
  // until the session was opened and closed again.
  // The group FOLLOWS the row it moved — the page the row lands on is the page
  // shown, and the row is brought back under the eye that starred it.
  const starredHere = useMemo(() => {
    const marked = new Set<string>();
    for (const session of sessions) {
      if (isFavorite(session)) marked.add(session.id);
    }
    return marked;
  }, [sessions]);
  const wasStarred = useRef(starredHere);
  const rowsRef = useRef<HTMLDivElement>(null);
  const following = useRef<string | null>(null);
  // Before paint: the reader must never see a frame of the page the row just left.
  useLayoutEffect(() => {
    const before = wasStarred.current;
    wasStarred.current = starredHere;
    // One tap flips one row. UNSTARRING moves a row just as far — down, out of the
    // pinned band — so it is followed the same way instead of being dropped
    // wherever the ordering sends it.
    const flipped =
      [...starredHere].find((id) => !before.has(id)) ??
      [...before].find((id) => !starredHere.has(id));
    if (!flipped) return;
    const index = sessions.findIndex((session) => session.id === flipped);
    if (index < 0) return;
    following.current = flipped;
    setPage(Math.floor(index / Math.max(1, Math.floor(pageSize) || 1)) + 1);
  }, [starredHere, sessions, pageSize]);
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
      {/* The project band wears the accent as its outgoing rule — one shelf per
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
          qualifier={homeifyPath(root) || 'No workspace path'}
          qualifierTitle={root}
          isOpen={isShowing}
          onToggle={() => fold(!isShowing)}
          label={`${isShowing ? 'Collapse' : 'Expand'} ${project}`}
        />
        {/* The trailing cluster now holds only what this group OFFERS. What it
            reports moved down to the shelf: on a 320px screen the count, the live
            pulse, the yellow verb and the `⋯` took this cluster's width first and
            left the project name 24px wide. */}
        <HeaderActions>
          <NewSessionButton
            machine={machineLabel(conn)}
            where={project}
            busyLabel={
              creating && creating.at === `${base}\u0000${root}` ? creating.label : null
            }
            onPress={() => onNewSession(root)}
            onDraft={onNewDraft ? (anchor) => onNewDraft(anchor, root) : undefined}
          />
        </HeaderActions>
      </SectionHeader>
      {/* The rows carry no bottom rule of their own: the trough that opens the next
          project, or the card's own bottom border, closes the group. */}
      {isShowing && rows.length > 0 && (
        <>
        {/* The group's own shelf, hung under its header and sticking with it: what
            the project counts, then the pages it is walked by. The pager used to
            stand at the FOOT of these rows, in the rows' paper, one hairline above
            the next project's header — a strip that read as a row of whichever of
            the two projects the eye picked. */}
        <SectionShelf>
          <HeaderMeta>
            <HeaderTally count={tally.count} unit="session" />
            <LiveCount count={tally.live} />
          </HeaderMeta>
          <Pager page={shownPage} pageCount={pageCount} onPage={setPage} label={`${project} sessions`} />
        </SectionShelf>
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
        </>
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
  // A draft is a per-session clone of the project; the row says so instead of
  // the list inventing a project for it.
  const draftName = isDraftWorkspace(session) ? session.workspace?.label?.trim() : '';

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
            Each fixed track is its own content's width: the status one holds the
            LONGEST label it can show, and `INPUT NEEDED` measures 83px against
            `LIVE`'s 34px, so it is 6rem/96px. The id track pays for those 16px —
            8 hex characters measure 48px inside 4.5rem/72px — instead of the
            title, which keeps 788px at 1440, 156px at 768 and 28px at 640. */}
        <span className="grid min-w-0 flex-1 grid-cols-[minmax(0,1fr)_auto_auto] items-center gap-x-3 gap-y-1 sm:grid-cols-[minmax(0,1fr)_5.5rem_4.5rem_4.5rem_6rem_6rem] sm:gap-y-0">
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
            {/* The star sits immediately RIGHT of the title, on every row: the strip
                that leaves the mark is under the row, so the mark itself has to be ON
                it. Riding at the end of the flag cluster it landed behind
                `new`/`dirty`/`draft`, moving with whatever else the row carried. */}
            {isStarred && (
              <span className="shrink-0">
                <StarIcon filled className="size-3" />
                <span className="sr-only">Favorite</span>
              </span>
            )}
          </span>
          {/* What the session HAS — unread answers, unsent words, the draft it was
              forked into — in ONE column of its own, so the flags of every
              row line up with each other instead of with the end of a title. On a
              phone the flags sit directly BESIDE the status they qualify — `NEW`
              against `IDLE` — which is what the fixed 6.75rem status track broke:
              it right-aligned four characters inside 108px and parked the badge 86px
              away from the word it belongs to. The flag track is `auto` and the
              status track next to it is fixed at the width of its longest label
              (`WAITING`), aligned to that track's END — the same right margin the
              timestamp on the line below stops on, so `NEW IDLE` and `7 hours ago`
              finish on one edge instead of the badge floating mid-row. The clock
              SPANS both columns rather than sharing the status one: sharing it, the
              wider of the two sized the track and pushed `IDLE` 48px away from the
              `NEW` it qualifies. */}
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
            {draftName !== '' && (
              <span
                className="shrink-0 border border-warn-strong px-1 font-mono text-chip font-bold uppercase tracking-[0.08em] text-warn-strong"
                title={session.workspace?.root}
              >
                draft {draftName}
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
            className={`col-start-3 row-start-1 inline-flex shrink-0 items-center gap-1 justify-self-end font-mono text-chip font-bold tracking-[0.08em] sm:col-start-auto sm:row-start-auto sm:justify-self-start ${statusTone(session)}`}
          >
            <span
              className={`size-1.5 shrink-0 ${statusDot(session)} ${live ? 'animate-pulse motion-reduce:animate-none' : ''}`}
            />
            {status}
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
      <dd className="truncate font-mono text-meta font-bold tabular-nums text-white">{value}</dd>
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
            {/* The shelf stands in too: a group is a band, a shelf and its rows, and
                a skeleton missing one of the three is a 36px jump the moment data
                lands. */}
            <SectionShelf>
              <SkeletonBar type="text-chip" width="w-20" baz="h-1.5" tone="bg-muted/25" />
            </SectionShelf>
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
                  <span className="grid min-w-0 flex-1 grid-cols-[minmax(0,1fr)_auto_auto] items-center gap-x-3 gap-y-1 sm:grid-cols-[minmax(0,1fr)_5.5rem_4.5rem_4.5rem_6rem_6rem] sm:gap-y-0">
                    <span className="col-start-1 row-start-1 sm:col-start-auto sm:row-start-auto">
                      <SkeletonBar type="text-meta" width={width} baz="h-2.5" tone="bg-muted/30" />
                    </span>
                    <span className="col-start-1 row-start-2 flex items-center gap-x-2 sm:contents">
                      <SkeletonBar type="text-chip" width="w-14" baz="h-1.5" tone="bg-muted/20" />
                      <SkeletonBar type="text-chip" width="w-10" baz="h-1.5" tone="bg-muted/20" />
                    </span>
                    {/* The flag column a real row keeps for `NEW` / `dirty` / a star.
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

function projectRoot(sessions: Session[]): string {
  return sessions.map(projectPath).find(Boolean) ?? '';
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

function relativeTime(value?: string): string {
  const millis = dateMillis(value);
  if (!millis) return '-';
  const seconds = Math.round((millis - Date.now()) / 1000);
  const absolute = Math.abs(seconds);
  const formatter = new Intl.RelativeTimeFormat(undefined, { numeric: 'auto' });
  if (absolute < 60) return formatter.format(seconds, 'second');
  if (absolute < 3_600) return formatter.format(Math.round(seconds / 60), 'minute');
  if (absolute < 86_400) return formatter.format(Math.round(seconds / 3_600), 'hour');
  if (absolute < 604_800) return formatter.format(Math.round(seconds / 86_400), 'day');
  return new Intl.DateTimeFormat(undefined, { month: 'short', day: 'numeric' }).format(millis);
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


// A parked draft says WHEN it forked, because that — not its name — is what tells
// you whether it still matches the project. A draft with no recorded fork time
// names its clone instead of inventing a date.
function draftHint(draft: WorkspaceDraft): string {
  const forked = draft.fork_ms
    ? relativeTime(new Date(draft.fork_ms).toISOString())
    : '';
  return forked ? `forked ${forked}` : homeifyPath(draft.root ?? '') || 'isolated workspace';
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
