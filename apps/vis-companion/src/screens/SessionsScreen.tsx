import { memo, useCallback, useDeferredValue, useEffect, useLayoutEffect, useMemo, useRef, useState } from 'react';
import {
  Banner,
  Button,
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
  EditableName,
  MachineGap,
  MachineMark,
  MachineRail,
  MachineSwitcher,
  MachineTab,
  Modal,
  NewSessionButton,
  RowDisclosure,
  SectionHeader,
  Spinner,
  UnreadBadge,
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
import type { GatewayConn, Session, SessionUsage, WorkspaceDraft } from '../lib/types';
import { homeifyPath } from '../lib/path';
import { onWake } from '../lib/wake';
import { seedReadMarks, unreadTurnCount, useReadMarks } from '../lib/unread';
import { assignMachineColors, machineColor } from '../lib/machine-colors';
import { menuPosition } from '../lib/anchored-menu';
import {
  applyListScroll,
  forgetListScroll,
  markListScroll,
  parkedListScroll,
  rememberListScroll,
  rowOffset,
  topVisibleRow,
  type ListAnchor,
} from '../lib/list-scroll';
import { SwipeActions } from '../components/SwipeActions';
import {
  ManageProjectsSheet,
  type ManagedProject,
} from '../components/ManageProjectsSheet';
import {
  PencilIcon,
  ProjectsIcon,
  SettingsIcon,
  StarIcon,
  TrashIcon,
} from '../components/icons';
import { DEFAULT_SESSION_PAGE_SIZE, getSessionsPerPage, subscribeSessionsPerPage } from '../lib/storage';
import { hostOf } from '../lib/endpoints';
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
import { favoriteKey, forgetFavorites, toggleFavorite, useFavorites } from '../lib/favorites';
import {
  groupByWorkDir,
  draftsRead,
  draftsReadKey,
  fleetError,
  isFleetLoaded,
  machineCounts,
  machineKey,
  machineLabel,
  newSessionTarget,
  projectDelete,
  reconcileMachines,
  scopedConns,
  scopedMachines,
  scopeError,
  searchTally,
  sessionIsListed,
  sessionIsLive,
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
  type StartFlow,
  type FleetMachine,
} from '../lib/fleet';

const SESSION_LIST_EVENTS = new Set([
  'turn.started',
  'turn.completed',
  'turn.failed',
  'turn.cancelled',
  'session.title_updated',
]);

// A background poll issued right before the OS suspended the webview can never
// settle: it neither resolves nor rejects after the resume. A plain in-flight
// boolean would then stay latched forever and every later refresh would be
// skipped — the list froze until the app was restarted. Anything older than
// this is treated as lost.
const STALE_POLL_MS = 20_000;

// A query can name more sessions than a phone will ever scroll. Hydrating the
// unloaded hits is one GET each, so the tail is cut rather than paid for.
const SEARCH_HYDRATE_MAX = 40;

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

// Rebuild the fleet from the paired machines, painting each new one from its own
// last known list so a machine that was on screen a second ago comes back with
// rows instead of a skeleton.
function hydrateMachines(conns: GatewayConn[], previous: FleetMachine[]): FleetMachine[] {
  return reconcileMachines(conns, previous).map((machine) => {
    if (machine.sessions !== null) return machine;
    const cached = clientFor(machine.conn).cachedSessions();
    return cached ? { ...machine, sessions: cached } : machine;
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
  /** Open that machine's own settings — the last verb in its `⋯` menu. */
  onMachineSettings?: (conn: GatewayConn) => void;
  /** Renames the machine in place from its own header. '' clears the name. */
  onRenameMachine?: (conn: GatewayConn, label: string) => void;
  /** Pair another machine. The `+` stands at the end of the tab strip. */
  onPair?: () => void;
}

export function SessionsScreen({
  conns,
  query,
  onQuery,
  subscriptions,
  onUnreachable,
  onOpen,
  onMachineSettings,
  onRenameMachine,
  onPair,
}: Props) {
  // A machine OWNS its projects: every row belongs to exactly one gateway, and a
  // project only exists inside the machine it lives on. The fleet is therefore
  // one entry per paired machine, seeded from that machine's last known list so
  // returning to this tab repaints the previous frame instantly; the effects
  // below revalidate each machine independently and reconcile on top.
  const [machines, setMachines] = useState<FleetMachine[]>(() => hydrateMachines(conns, []));
  // THE SCOPE IS ALWAYS EXACTLY ONE MACHINE — this one or that one, never "all".
  // A fleet-wide scope made every count, every verb and every create ask "which
  // machine?" all over again one row later; the switcher answers it once, up front.
  // The pick is only a PREFERENCE: it falls back to the first paired machine when it
  // names one that is gone, so `scope` below is null only while nothing is paired.
  const [scopePick, setScopePick] = useState<string | null>(null);
  const scope = machines.some((machine) => machineKey(machine.conn) === scopePick)
    ? scopePick
    : (machines[0] ? machineKey(machines[0].conn) : null);
  // Keep keystrokes immediate even when a large session fleet is regrouped.
  const deferredQuery = useDeferredValue(query);
  const [transcriptMatches, setTranscriptMatches] = useState<Map<string, SessionMatch> | null>(null);
  // Sessions a transcript hit named that this machine had not paged in yet, per
  // machine key. Kept beside the list instead of merged into it: the 10s poll
  // rewrites `machine.sessions` from the gateway's own paged answer.
  const [searchHits, setSearchHits] = useState<Map<string, Session[]>>(() => new Map());
  const [createBusy, setCreateBusy] = useState(false);
  const [createError, setCreateError] = useState<string | null>(null);
  const [manageProjects, setManageProjects] = useState<{
    machine: FleetMachine;
    at: { top: number; left: number };
  } | null>(null);
  // "Creating..." is a lie while a 12k-file repo is being cloned, so the busy word
  // follows the WORK: fork, enter, or plain create.
  const [createBusyLabel, setCreateBusyLabel] = useState('Creating...');
  // The `⋯` order — which machine, which workspace, what to call the draft — is ONE
  // value, so leaving it anywhere forgets every answer in it. The yellow button beside
  // it needs none of those answers: it starts where the machine already is. Portalled
  // and viewport-anchored because the header panel clips its overflow.
  const [startFlow, setStartFlow] = useState<StartFlow>(START_IDLE);
  // The menu and its draft sub-question share one surface, so both anchor from the
  // control the order started at. Browsing is NOT that surface: it takes the screen.
  const startMenu =
    startFlow.step === 'menu' || startFlow.step === 'drafts' ? startFlow.at : null;
  const browseAt = startFlow.step === 'browse' ? startFlow.at : null;
  // Forking asks for the draft's name first: the gateway rejects a blank label, and
  // the name is what `/draft list` and every later resume will show.
  const namePrompt = startFlow.step === 'name' ? startFlow : null;
  // The control the open order hangs from — a machine header's `⋯`, its New session
  // button when that machine has no project yet, or the solo fleet bar's. An element,
  // not a ref, because every header carries its own pair.
  const startAnchorEl = useRef<HTMLElement | null>(null);
  // One entry per machine+repo, kept across openings; see `forgetParkedDrafts`.
  const [draftsCache, setDraftsCache] = useState<
    Record<string, { rows: WorkspaceDraft[]; error: string | null }>
  >({});
  const [draftLabel, setDraftLabel] = useState('');
  const pollStartedAt = useRef<number | null>(null);
  // Swipe-revealed row actions, plus the group header's project delete. One dialog
  // serves all three: renaming asks for the new title, both deletes ask for consent
  // — a destructive tap two pixels from a thumb rest position must never be one-way.
  const [rowAction, setRowAction] = useState<RowAction | null>(null);
  const [renameDraft, setRenameDraft] = useState('');
  const [actionBusy, setActionBusy] = useState(false);
  const [actionError, setActionError] = useState<string | null>(null);
  // Fan-out progress. Deleting a group that is not a project row is one request per
  // session, and forty of them behind a motionless 'Deleting...' is indistinguishable
  // from a hang.
  const [actionProgress, setActionProgress] = useState<{ done: number; total: number } | null>(null);
  const listRef = useRef<HTMLDivElement>(null);
  const refreshAnchorRef = useRef<ListAnchor | null>(null);
  // The reading position is put back at most once per mount, and never after the
  // reader has taken the scroller over.
  const restoredRef = useRef(false);
  const connsRef = useRef(conns);
  const machinesRef = useRef(machines);
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
  // Stars outrank every heuristic below, so the list repaints when one moves.
  const favorites = useFavorites();

  // A session this device has never met is NOT unread: seed it at the turn count
  // it arrived with, so only answers that land AFTER this point raise a badge.
  // Without the seed, a fresh install would paint the whole fleet unread.
  useEffect(() => {
    for (const machine of machines) if (machine.sessions) seedReadMarks(machine.sessions);
  }, [machines]);

  const patchMachine = useCallback(
    (key: string, update: (machine: FleetMachine) => FleetMachine) => {
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

  // ONE machine's list. Machines load independently on purpose: a gateway that is
  // asleep must not keep the machines next to it off the screen, and its failure
  // degrades that machine's section instead of the whole list.
  const loadMachine = useCallback(
    async (conn: GatewayConn, signal?: AbortSignal) => {
      const key = machineKey(conn);
      const api = clientFor(conn);
      try {
        // Paint the first page the moment it lands instead of waiting for the whole
        // fleet to drain. Only ever called on a cold load (see `listSessions`).
        const next = await api.listSessions(signal, (partial) => {
          if (signal?.aborted) return;
          patchMachine(key, (machine) => ({
            ...machine,
            sessions: reconcileSessions(machine.sessions, partial),
            error: null,
          }));
        });
        if (signal?.aborted) return;
        // Anchor EVERY reload: the 10s poll can reorder rows under a reading
        // thumb. The layout effect below no-ops at the top of the list, so the
        // first paint is unaffected.
        refreshAnchorRef.current = topVisibleRow(listRef.current);
        patchMachine(key, (machine) => ({
          ...machine,
          sessions: reconcileSessions(machine.sessions, next),
          error: null,
        }));
      } catch (cause) {
        if (signal?.aborted) return;
        patchMachine(key, (machine) => ({ ...machine, error: (cause as Error).message }));
      }
    },
    [patchMachine],
  );

  const load = useCallback(
    async (signal?: AbortSignal, background = false) => {
      if (background) {
        const started = pollStartedAt.current;
        if (started !== null && Date.now() - started < STALE_POLL_MS) return;
        pollStartedAt.current = Date.now();
      }
      try {
        await Promise.all(connsRef.current.map((conn) => loadMachine(conn, signal)));
      } finally {
        if (background) pollStartedAt.current = null;
      }
    },
    [loadMachine],
  );

  // A machine's NAME is not its transport, so renaming it must not refetch a thing —
  // but the banner reads that name off `machine.conn`, and `fleetKey` deliberately
  // ignores it, so an in-place rename saved to storage and then painted the old name
  // until the next pairing change. Re-hydrating keeps every row (`reconcileMachines`
  // hands the surviving machine its new connection) and reloads nothing.
  const fleetNames = conns.map((conn) => `${conn.url}\u0000${conn.label ?? ''}`).join('|');
  useEffect(() => {
    setMachines((current) => hydrateMachines(connsRef.current, current));
  }, [fleetNames]);

  // Pairing changes rebuild the fleet; machines that stayed keep their rows.
  useEffect(() => {
    setMachines((current) => hydrateMachines(connsRef.current, current));
  }, [fleetKey]);

  useEffect(() => {
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
  }, [fleetKey, load]);

  useEffect(() => {
    if (!subscriptions) return;
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
  }, [load, subscriptions]);

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

  useLayoutEffect(() => {
    const viewport = listRef.current;
    if (!viewport) return;
    // The reader scrolling is the reader deciding: stop trying to restore.
    const abandon = () => {
      restoredRef.current = true;
      forgetListScroll();
    };
    viewport.addEventListener('wheel', abandon, { passive: true });
    viewport.addEventListener('touchstart', abandon, { passive: true });
    return () => {
      viewport.removeEventListener('wheel', abandon);
      viewport.removeEventListener('touchstart', abandon);
      // A layout cleanup still runs against the live DOM, which is the last
      // moment this scroller can be measured at all.
      if (viewport.isConnected) rememberListScroll(markListScroll(viewport, topVisibleRow(viewport)));
    };
  }, []);

  // Transcript search runs server-side (matches user requests + LLM responses)
  // and unions its matching ids into the local title/project filter.
  useEffect(() => {
    const needle = deferredQuery.trim();
    // An empty query has no server matches; `matches` below derives that without
    // writing state from this effect.
    if (!needle) return;
    const targets = scopedConns(connsRef.current, scope);
    if (targets.length === 0) return;
    const controller = new AbortController();
    const timer = window.setTimeout(() => {
      void Promise.all(
        // One machine failing to search (asleep, older gateway) must not blank the
        // matches the others found.
        targets.map(async (conn) => {
          const key = machineKey(conn);
          const api = clientFor(conn);
          const found = await api
            .searchSessionMatches(needle, controller.signal)
            .catch(() => []);
          // The list is PAGED. Intersecting the hits with the rows already loaded
          // meant search could only find what was on screen; a hit in a session
          // further down the fleet's ordering vanished. Fetch those rows by id.
          const loaded = new Set(
            (machinesRef.current.find((machine) => machineKey(machine.conn) === key)
              ?.sessions ?? []).map((session) => session.id),
          );
          const missing = found
            .filter((match) => !loaded.has(match.sessionId))
            .slice(0, SEARCH_HYDRATE_MAX);
          const rows = await Promise.all(
            missing.map((match) =>
              api.session(match.sessionId, controller.signal).catch(() => null),
            ),
          );
          return {
            key,
            found,
            rows: rows.filter((row): row is Session => row !== null),
          };
        }),
      ).then((results) => {
        if (controller.signal.aborted) return;
        setTranscriptMatches(
          new Map(
            results.flatMap((result) => result.found).map((match) => [match.sessionId, match]),
          ),
        );
        setSearchHits(new Map(results.map((result) => [result.key, result.rows])));
      });
    }, 200);
    return () => {
      controller.abort();
      window.clearTimeout(timer);
    };
  }, [deferredQuery, fleetKey, scope]);

  const matches = deferredQuery.trim() ? transcriptMatches : null;
  const searching = deferredQuery.trim().length > 0;

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
    const needle = deferredQuery.trim().toLowerCase();
    return inScope.map((machine) => {
      const base = clientFor(machine.conn).base;
      const draftFor = (session: Session) => draftMessages[draftMessageKey(base, session.id)];
      const rankFor = (session: Session) => favorites[favoriteKey(base, session.id)] ?? null;
      // Server-side transcript hits this machine had not paged in are part of the
      // list a query filters: without them search only finds what is on screen.
      const hits = needle ? (searchHits.get(machineKey(machine.conn)) ?? []) : [];
      const sessions = withSearchHits(machine.sessions ?? [], hits).filter((session) => {
        const draft = draftFor(session);
        const listed = sessionIsListed(session, {
          hasDraftMessage: draftMessageHasUnsent(draft),
          isFavorite: rankFor(session) !== null,
        });
        if (!listed) return false;
        return (
          !needle ||
          sessionSearchText(session).includes(needle) ||
          // A dirty row has no title and no transcript: what waits in its composer
          // — the words AND the names of the files staged with them — is the only
          // thing a query could match it on.
          draftSearchText(draft).includes(needle) ||
          matches?.has(session.id) === true
        );
      });
      // Starred rows first, then unsent work, and with them the project group that
      // owns them: see `sessionOrder`.
      return {
        machine,
        sessions: sessionOrder(sessions, {
          favoriteRank: rankFor,
          hasDraftMessage: (session) => draftMessageHasUnsent(draftFor(session)),
        }),
      };
    });
  }, [inScope, deferredQuery, matches, searchHits, draftMessages, favorites]);

  // A filter is a FLEET question: it runs on every machine in scope, so the header
  // reports what came back and from how many of them.
  const searchCounts = useMemo(() => searchTally(filtered), [filtered]);

  const visible = useMemo(
    () => (sessions === null ? null : filtered.flatMap((entry) => entry.sessions)),
    [filtered, sessions],
  );

  const totals = useMemo(() => {
    const all = sessions?.length ?? 0;
    const shown = visible?.length ?? 0;
    // Projects are counted PER MACHINE. Counting bare folder names collapsed two
    // machines' `vis` checkouts into one project that belonged to neither.
    const projects = new Set(
      inScope.flatMap((machine) =>
        (machine.sessions ?? []).map((session) => `${machineKey(machine.conn)}\u0000${projectPath(session)}`),
      ),
    ).size;
    const live = sessions?.filter(sessionIsLive).length ?? 0;
    const unread = sessions?.filter((session) => unreadTurnCount(session) > 0).length ?? 0;
    return { all, shown, projects, live, unread };
    // `readMarks` is the store version: marks change outside React, so it is the
    // dependency that makes the unread tally recompute.
  }, [inScope, sessions, visible, readMarks]);

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
  // THE MACHINE IS SELECTION, NOT STRUCTURE.
  // The list used to carry two bands one hairline apart — a machine header and a
  // project header, same x, same trailing cluster — so nothing said the second was
  // inside the first. The chip strip answers "which machine", the chrome above the
  // list NAMES the one in scope and carries its verbs, and the list below holds one
  // header kind. `null` only while the bar speaks for several machines at once.
  const scopeChrome = scopeMachine;

  // One hue per paired machine, assigned from the machine's own key, so a rail
  // keeps its colour across reloads and reorderings and two machines side by side
  // never share one. Colour is what the eye reads before the name, and the same
  // hue rides the scope chip above the list and the rail down its left.
  const machineColors = useMemo(
    () => assignMachineColors(machines.map((machine) => machineKey(machine.conn))),
    [machines],
  );

  // A scope narrowed to a dead machine is not an empty machine: with the rest of
  // the fleet hidden, that machine's failure IS the screen.
  const scopedError = scopeError(machines, scope);

  const selectScope = useCallback((next: string) => {
    setScopePick(next);
    // Drafts are repo-scoped ON a machine, and the scope is what decides which
    // machine the next session lands on: the open order asks a question that just
    // changed underneath it, so it ends rather than answering the old one.
    setStartFlow(START_IDLE);
  }, []);

  /**
   * Drafts are REPO-scoped, and the gateway only lists them through a session that
   * lives in that repo. The picker therefore reads them off the most recent session
   * that has a workspace, and the menu NAMES that repo — a fleet spanning several
   * projects must not be told these drafts belong to whatever it creates next.
   */
  // Which machine the `⋯` order is about while the bar speaks for the whole fleet: the
  // scoped machine, or the only one paired. `null` means the app must ASK before it
  // can create anything.
  const scopeTarget = newSessionTarget(machines, scope);
  // Several machines: the menu asks WHICH first, and that answer — not a session on
  // trunk — is what the workspace question below is then asked about.
  const ask = startAsk(machines, scopeTarget, startFlowOn(startFlow));
  const target = ask.on;
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
  const draftRepo = draftProbe ? projectLabel(draftProbe) : '';
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
  // else: the verb menu never waits on a list it does not show.
  const isDraftsOpen = startFlow.step === 'drafts';
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
   * Open the machine's menu under the control that was tapped — a machine header's
   * `⋯`, or the solo fleet bar's. Passing `on` ANSWERS which machine at the same
   * time, because a header can only ever mean its own gateway.
   */
  const openStartMenuAt = useCallback(
    (anchor: HTMLElement | null, on: GatewayConn | null = null) => {
      if (anchor) startAnchorEl.current = anchor;
      const at = menuPosition(startAnchorEl.current?.getBoundingClientRect(), MENU_WIDTH);
      setStartFlow((flow) => {
        const opened = startFlowOpen(flow, at);
        return on && opened.step === 'menu' ? startFlowPick(opened, on) : opened;
      });
    },
    [],
  );

  // Re-anchoring an OPEN menu keeps every answer in it: a resize is not an answer, and
  // on a phone the keyboard fires one in the very tap that opens it.
  const openStartMenu = useCallback(() => openStartMenuAt(null), [openStartMenuAt]);

  /**
   * The draft half of a project header's split button: one tap lands on the draft
   * question with machine AND project already answered. It used to be a row inside the
   * machine's `⋯`, two headers above the project it actually forks.
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
    if (!isDraftsOpen || isDraftsRead) return;
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
  }, [isDraftsOpen, isDraftsRead, draftsSourceKey]);

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
    setCreateBusy(true);
    setCreateBusyLabel(
      startIn.kind === 'fork' ? 'Forking...' : startIn.kind === 'resume' ? 'Entering...' : 'Creating...',
    );
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
      await load();
      if (session.id) await onOpen(on, session.id, true);
    } catch (cause) {
      setCreateError((cause as Error).message);
    } finally {
      setCreateBusy(false);
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
  function closeRowAction() {
    setRowAction(null);
    setActionError(null);
    setActionProgress(null);
  }

  async function commitRowAction() {
    if (!rowAction) return;
    const api = clientFor(rowAction.conn);
    // The words that made a row dirty die with it: a draft message kept under a
    // session id that no longer exists is unreachable forever, and a star on a
    // session nobody can open is the same litter.
    const forgetDrafts = (ids: string[]) => {
      for (const sid of ids) clearDraftMessage(draftMessageKey(api.base, sid));
      forgetFavorites(ids.map((sid) => favoriteKey(api.base, sid)));
      if (ids.length > 0) void flushDraftMessages();
    };
    const title = renameDraft.trim();
    if (rowAction.mode === 'rename' && !title) {
      setActionError('A session name cannot be empty.');
      return;
    }
    setActionBusy(true);
    setActionError(null);
    try {
      if (rowAction.mode === 'rename') await api.renameSession(rowAction.session.id, title);
      else if (rowAction.mode === 'delete') {
        await api.deleteSession(rowAction.session.id);
        forgetDrafts([rowAction.session.id]);
      } else {
        const plan = projectDelete(rowAction.sessions);
        // One recursive request when a project row owns the whole group: the gateway
        // deletes the members it knows about, which is more than this list paints.
        if (plan.kind === 'project') forgetDrafts(await api.deleteProject(plan.projectId));
        else {
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
          if (failed > 0) {
            setActionError(`${failed} of ${plan.sessionIds.length} sessions could not be deleted.`);
            await load();
            return;
          }
        }
      }
      setRowAction(null);
      await load();
    } catch (cause) {
      setActionError((cause as Error).message);
    } finally {
      setActionBusy(false);
      setActionProgress(null);
    }
  }

  const rowCopy = rowAction ? rowActionCopy(rowAction, machineLabel(rowAction.conn)) : null;

  const pageSize = useSessionsPerPage();

  // Machine → project → sessions. The machine is the organizer, so its sections
  // are built from ITS rows only.
  const sections = useMemo(
    () =>
      filtered.map((entry) => ({
        machine: entry.machine,
        // Group identity and every create action keep the gateway's canonical path.
        // Home-shortening is paint only; feeding `~/vis` back as an API root is how an
        // older gateway produced the impossible `/…/vis/~/vis` directory.
        groups: groupByWorkDir(entry.sessions),
      })),
    [filtered],
  );

  // The projects the "remove sessions" step offers are the ones this machine is
  // SHOWING, read from the same grouping the list renders — a row that promises to
  // remove 975 transcripts under a header reading 712 is a row nobody should press.
  const managedProjects = useCallback(
    (machine: FleetMachine): ManagedProject[] =>
      groupByWorkDir(machine.sessions ?? []).map(([, sessions]) => ({
        name: projectLabel(sessions[0]!),
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
          raised tile inside it. There is no "All": a scope is one machine, always. */}
      <div
        role="group"
        aria-label="Machines"
        className="relative z-10 flex items-center gap-1.5 px-3 pb-3 pt-6 sm:px-4 sm:pb-4 sm:pt-8"
      >
          <MachineSwitcher>
          {machines.map((machine) => {
            const key = machineKey(machine.conn);
            const tally = tallies.get(key);
            return (
              <MachineTab
                key={key}
                isOn={scope === key}
                onClick={() => selectScope(key)}
              >
                <MachineMark color={machineColor(machineColors, key)} />
                {machineLabel(machine.conn)}
                {machine.error ? (
                  <span className="opacity-70">offline</span>
                ) : (
                  <>
                    {/* A COUNT INSIDE A TILE IS INK, NOT A THIRD BOX. The live tally
                        was a filled green pill inside a bordered chip inside a row:
                        boxes three deep in 32px. Live is the machine's resting state,
                        so it is plain ink; unread is news and keeps its amber block. */}
                    {(tally?.live ?? 0) > 0 && (
                      <span className="tabular-nums opacity-70">
                        {tally?.live}
                        <span className="sr-only"> live</span>
                      </span>
                    )}
                    <UnreadBadge count={tally?.unread ?? 0} />
                  </>
                )}
              </MachineTab>
            );
          })}
          </MachineSwitcher>
          {/* ADDING A MACHINE IS THE TAB STRIP'S OWN VERB, AND IT IS A WORD.

              The strip answers "which machine", so "one more machine" belongs at its
              end, not buried in Preferences. A bare `+` beside named tabs asks the
              reader to guess what it adds - a project? a session? - so the control
              says `Add machine`, on the page's own paper, never inside the card it
              would add a sibling to. `ml-auto` puts it on the row's TRAILING edge:
              tabs read left to right and the verb is not one of them. It is `inverse`
              — the page's own ink, poured — so it reads as a pressable control beside
              the amber primaries in the card below without becoming a second one. */}
          {onPair && (
            <Button
              variant="inverse"
              density="compact"
              className="ml-auto shrink-0 whitespace-nowrap"
              onClick={onPair}
            >
              Add machine
            </Button>
          )}
      </div>
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
        <div className="flex h-full min-h-0 flex-col overflow-hidden border-y border-dialog-edge bg-panel sm:mx-0 sm:h-auto sm:max-h-full sm:border-x sm:border-r-2">
        <div className={`border-b border-dialog-edge bg-panel-2 px-3 py-2 sm:px-4 ${LIST_FRAME}`}>
          <div className="mt-1.5 flex items-center justify-between gap-3">
            <div className="min-w-0">
              {/* The machine in scope is NAMED here, once, and the name is the rename
                  control: the band that used to carry it is gone, so this is where a
                  human owns the word. Same box editing or resting — no jump. */}
              {scopeChrome && onRenameMachine ? (
                <EditableName
                  className="truncate font-mono text-ui font-bold text-white"
                  label={`Rename ${machineLabel(scopeChrome.conn)}`}
                  value={machineLabel(scopeChrome.conn)}
                  onCommit={(next) => onRenameMachine(scopeChrome.conn, next)}
                />
              ) : (
                <p className="truncate font-mono text-ui font-bold text-white">
                  {scopeChrome ? machineLabel(scopeChrome.conn) : 'Fleet'}
                </p>
              )}
              <p className="mt-0.5 flex flex-wrap items-center gap-x-3 gap-y-0.5 font-mono text-chip text-dialog-hint">
                {sessions === null ? (
                  <>
                    <Spinner className="text-accent-ink" />
                    <span>Reading sessions...</span>
                  </>
                ) : scopedError ? (
                  <span className="whitespace-nowrap font-bold text-accent-ink">not answering</span>
                ) : (
                  <>
                    {/* Facts travel as WHOLE units. Every value used to be its own flex
                        child with a bare `·` child between them, so a wrap left the
                        separator dangling at the end of the line ("447 sessions ·") and
                        could strand "●" from its "4 live". Each fact is now nowrap and
                        the groups are separated by SPACE rather than punctuation, so the
                        line can only break between facts. */}
                    {searching ? (
                      <>
                        <span className="whitespace-nowrap font-bold text-accent-ink">
                          {searchCounts.matches} {searchCounts.matches === 1 ? 'match' : 'matches'}
                        </span>
                      </>
                    ) : (
                      <>
                        <span className="whitespace-nowrap">
                          {totals.projects} {totals.projects === 1 ? 'project' : 'projects'}
                          <span className="px-1" aria-hidden="true">·</span>
                          {totals.all} {totals.all === 1 ? 'session' : 'sessions'}
                        </span>
                        {/* WHERE the two numbers live is a one-place question.
                            The scope strip is always on screen, and every chip
                            carries its machine's live and unread while the All
                            chip carries the fleet's — one row below this line.
                            Saying it here too was the same fact twice. */}
                      </>
                    )}
                  </>
                )}
              </p>
            </div>
            <div className="flex shrink-0 items-center gap-2">
              {createBusy && (
                <span aria-live="polite" className="font-mono text-chip text-dialog-hint">
                  {createBusyLabel}
                </span>
              )}
              {/* The machine's two verbs, on the chrome that names it rather than on a
                  band of its own: ADD a project, and open this machine's settings. With
                  several machines speaking at once there is no machine to act on — a
                  workspace only exists on one — so the chip is asked first.
                  Both are BUTTONS with a face. They were `quiet`, which is deliberately
                  frameless, so two words sat on the chrome as bare ink beside an amber
                  `New session` slab and nothing said they could be pressed. ADD is the
                  amber primary here, the same fill the list's own create verb wears;
                  settings is the framed sibling, so one amber never rivals another. */}
              {scopeChrome && !scopeChrome.error && (
                <Button
                  variant="solid"
                  density="compact"
                  className="shrink-0 whitespace-nowrap"
                  aria-label={`Add a project on ${machineLabel(scopeChrome.conn)}`}
                  onClick={(event) => {
                    const at = menuPosition(
                      event.currentTarget.getBoundingClientRect(),
                      BROWSE_WIDTH,
                    );
                    if (!at) return;
                    setManageProjects({ machine: scopeChrome, at });
                  }}
                >Add project</Button>
              )}
              {scopeChrome && onMachineSettings && (
                <Button
                  variant="ghost"
                  density="compact"
                  className="shrink-0 whitespace-nowrap"
                  aria-label={`Settings for ${machineLabel(scopeChrome.conn)}`}
                  onClick={() => onMachineSettings(scopeChrome.conn)}
                >Machine settings</Button>
              )}
            </div>
          </div>
          {createError && (
            <div className="mt-2">
              <Banner kind="err">{createError}</Banner>
            </div>
          )}
        </div>

        <div ref={listRef} className="min-h-0 flex-1 touch-pan-y overflow-x-hidden overflow-y-auto overscroll-contain [overflow-anchor:auto] [scrollbar-gutter:stable]">
        {sessions === null ? (
          <NavigatorSkeleton />
        ) : scopedError && !visible?.length ? (
          <div className={`px-5 py-16 text-center ${LIST_FRAME}`}>
            <p className="font-mono text-body font-bold text-white/70">
              {scopeMachine ? `${machineLabel(scopeMachine.conn)} is not answering` : 'No machine is answering'}
            </p>
            <p className="mt-2 font-mono text-ui text-dialog-hint">{scopedError}</p>
            <div className="mt-4 flex justify-center">
              <Button variant="ghost" onClick={() => void load()}>
                Retry
              </Button>
            </div>
          </div>
        ) : visible?.length === 0 ? (
          <div className={`px-5 py-16 text-center ${LIST_FRAME}`}>
            <p className="font-mono text-body font-bold text-white/70">
              {query ? 'No matching sessions' : 'No sessions yet'}
            </p>
            <p className="mt-2 font-mono text-ui text-dialog-hint">
              {query
                ? 'Nothing on any paired machine matches that.'
                : 'Open the ⋯ menu to start one.'}
            </p>
            {/* The field is in the app bar now, a screen away from this sentence, so the
                way back to a full list is offered where the dead end is. */}
            {query && (
              <div className="mt-4 flex justify-center">
                <Button variant="ghost" onClick={() => onQuery('')}>
                  Clear search
                </Button>
              </div>
            )}
          </div>
        ) : (
          <div>
            {sections.map(({ machine, groups }, index) => {
              const key = machineKey(machine.conn);
              return (
                <section key={key} aria-label={`${machineLabel(machine.conn)} projects`}>
                  {/* Every machine keeps its own named panel and landmark, even when it is
                      the only machine in the fleet. */}
                  {/* A machine boundary is not a project boundary, so it is not drawn
                      with the same hairline: a band of the page's own colour, closed top
                      and bottom by the strong rule, says one computer ENDED before any
                      label is read. It is charged once per EXTRA machine — the first
                      block starts flush, and a solo fleet never pays it. */}
                  {index > 0 && <MachineGap />}
                  {/* Everything one machine owns hangs off ITS rail, and that rail IS
                      the card's left frame here — a project boundary is a hairline, a
                      machine boundary is a colour change, so where `tower` ends is seen
                      before it is read. The panel is always rendered for the machine
                      whose projects follow, fleet view or scoped view alike. */}
                  <MachineRail color={machineColor(machineColors, key)}>
                  {groups.length === 0
                    ? (
                        <div className="flex flex-wrap items-center gap-3 px-3 py-3 sm:px-4">
                          <p className="font-mono text-meta text-dialog-hint">
                            {machine.error
                              ? `${machineLabel(machine.conn)} is not answering.`
                              : machine.sessions === null
                                ? 'Reading sessions...'
                                : searching
                                  ? 'No matches on this machine.'
                                  : 'No sessions on this machine yet.'}
                          </p>
                          {/* The band that used to carry this machine's Retry is gone, so
                              the offer stands where its sessions would have been. */}
                          {machine.error && (
                            <Button
                              type="button"
                              variant="quiet"
                              density="compact"
                              pressEffect="none"
                              onClick={() => void loadMachine(machine.conn)}
                            >
                              Retry
                            </Button>
                          )}
                        </div>
                      )
                    : groups.map(([groupRoot, projectSessions]) => (
                        <ProjectGroup
                          key={`${key}\u0000${groupRoot}`}
                          project={projectLabel(projectSessions[0]!)}
                          sessions={projectSessions}
                          conn={machine.conn}
                          matches={matches}
                          needle={deferredQuery.trim()}
                          onOpen={onOpen}
                          onRename={startRename}
                          onDelete={startDelete}
                          onNewSession={(root) => void createSession({ kind: 'trunk' }, machine.conn, root)}
                          // A draft is not a preference: every project header offers
                          // the private copy beside its own "New session".
                          onNewDraft={(anchor, root) => openDraftsAt(anchor, machine.conn, root)}
                          pageSize={pageSize}
                          drafts={draftMessages}
                        />
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

      {rowAction && rowCopy && (
        <Modal onDismiss={closeRowAction}>
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
                  <Button variant="ghost" onClick={closeRowAction}>
                    Cancel
                  </Button>
                  <Button
                    variant={rowAction.mode === 'rename' ? 'solid' : 'danger'}
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
          label={
            target
              ? isDraftsOpen
                ? 'Start the new session in'
                : `Actions for ${machineLabel(target)}`
              : 'Create the new session on'
          }
          at={startMenu}
          onDismiss={() => leaveStart(true)}
        >
          {target && !isDraftsOpen ? (
            /* What is LEFT once the verb moved out to the project header: the yellow
               button there already starts a session in the project it names, so this
               menu holds the rarer half of the order — a private copy, this machine's
               files, the machine itself. */
            <>
              <MenuHeading>{machineLabel(target)}</MenuHeading>
              {/* Managing a machine's project folders is a machine verb, so it is
                  BEHIND this control with the other ones instead of sitting beside it
                  as a second word-button in the header's right corner. */}
              <MenuItem
                icon={<ProjectsIcon className="size-4" />}
                title="Manage projects"
                hint="add, choose, and remove this machine's projects"
                onSelect={(anchor) => {
                  const at = menuPosition(anchor.getBoundingClientRect(), BROWSE_WIDTH);
                  if (!at || !targetMachine) return;
                  leaveStart();
                  setManageProjects({ machine: targetMachine, at });
                }}
              />
              {onMachineSettings && (
                <MenuItem
                  icon={<SettingsIcon className="size-4" />}
                  title="Machine settings"
                  hint="name, pairing, unpair"
                  onSelect={() => {
                    leaveStart();
                    onMachineSettings(target);
                  }}
                />
              )}
            </>
          ) : target ? (
            <>
              {/* A step INSIDE the same order, so it is left the way it was entered:
                  back to the machine's verbs, never out to a blank screen. */}
              <MenuBack
                label={`Back to actions for ${machineLabel(target)}`}
                onBack={() => setStartFlow(startFlowBack)}
              >
                Start the session in
                {machines.length > 1 ? ` · ${machineLabel(target)}` : ''}
              </MenuBack>
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
                  <Spinner className="text-accent-ink" />
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

      {/* The folder browser the start flow falls through to when a machine has no
          project yet. `manageProjects` below is the same sheet reached deliberately
          from a machine's `⋯`. */}
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
          isAdding
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
                  <Button variant="ghost" onClick={() => leaveStart()}>
                    Cancel
                  </Button>
                  <Button
                    variant="solid"
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

// Memoised: a 5.5s poll that changes nothing returns the SAME row objects
// (`reconcileSessions`), so an unchanged group must not re-render its rows.
const ProjectGroup = memo(function ProjectGroup({
  project,
  sessions,
  conn,
  matches,
  needle,
  drafts,
  onOpen,
  onRename,
  onDelete,
  onNewSession,
  onNewDraft,
  pageSize,
}: {
  project: string;
  sessions: Session[];
  conn: GatewayConn;
  matches: Map<string, SessionMatch> | null;
  needle: string;
  /** Unsent composer content for the whole fleet; each row reads its own entry. */
  drafts: DraftMessageStore;
  onOpen: Props['onOpen'];
  onRename: (session: Session, conn: GatewayConn) => void;
  onDelete: (session: Session, conn: GatewayConn) => void;
  onNewSession: (root: string) => void;
  /** Opens the private-copy question for this project, anchored on the button. */
  onNewDraft?: (anchor: HTMLElement, root: string) => void;
  pageSize: number;
}) {
  const root = projectRoot(sessions);
  const base = useMemo(() => clientFor(conn).base, [conn]);
  const liveCount = useMemo(() => sessions.filter(sessionIsLive).length, [sessions]);
  // Row actions must reach the machine that OWNS the row. Bound here so a
  // memoized row does not re-render on every paint of its parent.
  const renameRow = useCallback((session: Session) => onRename(session, conn), [onRename, conn]);
  const deleteRow = useCallback((session: Session) => onDelete(session, conn), [onDelete, conn]);

  // A project is WALKED, page by page, and the gateway cuts the pages
  // (`listProjectPage`, `root=`). Page 1 is painted from rows the fleet poll has
  // already delivered, so the first screen costs no request; every later page is
  // the server's own window of this project, which is the only way a project with
  // a thousand sessions can be read at all.
  const [page, setPage] = useState(1);
  const pageCount = Math.max(1, Math.ceil(sessions.length / pageSize));
  useEffect(() => {
    // The fleet moved under the pager (a deletion, a filter, a smaller step): the
    // page that no longer exists becomes the first one rather than an empty band.
    if (page > pageCount) setPage(1);
  }, [page, pageCount]);

  const localRows = useMemo(
    () => sessions.slice((page - 1) * pageSize, page * pageSize),
    [sessions, page, pageSize],
  );

  const [serverRows, setServerRows] = useState<Session[] | null>(null);
  useEffect(() => {
    // A search is a FLEET question answered client-side over rows already held;
    // asking the gateway for an unfiltered window would page past the matches.
    if (page === 1 || needle) {
      setServerRows(null);
      return;
    }
    let alive = true;
    const controller = new AbortController();
    void clientFor(conn)
      .listProjectPage(root, (page - 1) * pageSize, pageSize, controller.signal)
      .then((window) => {
        if (alive) setServerRows(window.rows);
      })
      // The locally sliced page is a truthful fallback: same ordering, same rows.
      .catch(() => undefined);
    return () => {
      alive = false;
      controller.abort();
    };
  }, [conn, root, page, pageSize, needle]);

  const rows = serverRows ?? localRows;

  return (
    <>
    <section aria-label={`${project} sessions`}>
      <SectionHeader>
        {/* The leading half only NAMES the project now: its folder name and the path
            that tells two `vis` checkouts apart. It was a disclosure button, which
            hid a whole project's history behind a tap and said nothing about how
            much of it there was; the history is PAGED below instead. */}
        <HeaderTitle
          name={project}
          qualifier={homeifyPath(root) || 'No workspace path'}
          qualifierTitle={root}
        />
        {/* The same trailing cluster the machine header above wears: what this group
            reports, then what it offers — the yellow verb gets its gap, the `⋯` gets
            the same box, and both stop at the same right edge as every other header. */}
        <HeaderActions>
          <HeaderMeta>
            <HeaderTally count={sessions.length} unit="session" />
            <LiveCount count={liveCount} />
          </HeaderMeta>
          <NewSessionButton
            machine={machineLabel(conn)}
            where={project}
            onPress={() => onNewSession(root)}
            onDraft={onNewDraft ? (anchor) => onNewDraft(anchor, root) : undefined}
          />
        </HeaderActions>
      </SectionHeader>
      {/* The list carries no bottom rule of its own: the card's own bottom border
          closes it, and the two of them stacked into a doubled line under the pager. */}
      {rows.length > 0 && (
        <div>
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
              onDelete={deleteRow}
            />
          ))}
          <Pager page={page} pageCount={pageCount} onPage={setPage} label={`${project} sessions`} />
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
  onDelete,
}: {
  session: Session;
  /** This device's unsent composer content for the session; EMPTY when there is none. */
  draft: DraftMessage;
  conn: GatewayConn;
  match: SessionMatch | null;
  needle: string;
  onOpen: Props['onOpen'];
  onRename: (session: Session) => void;
  onDelete: (session: Session) => void;
}) {
  const status = statusLabel(session);
  const timestamp = session.modified_at ?? session.last_active_at ?? session.created_at;
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
  // timestamp cannot announce.
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
  const favorites = useFavorites();
  const starKey = favoriteKey(clientFor(conn).base, session.id);
  const isStarred = starKey in favorites;
  // A draft is a per-session clone of the project; the row says so instead of
  // the list inventing a project for it.
  const draftName = isDraftWorkspace(session) ? session.workspace?.label?.trim() : '';

  return (
    <div className="[&+&]:border-t [&+&]:border-dialog-edge">
      <SwipeActions
        label={title}
        actions={[
          {
            key: 'favorite',
            label: isStarred ? 'Unstar' : 'Star',
            icon: <StarIcon filled={isStarred} className="size-4" />,
            onSelect: () => toggleFavorite(starKey),
          },
          {
            key: 'rename',
            label: 'Rename',
            icon: <PencilIcon className="size-4" />,
            onSelect: () => onRename(session),
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
            x=1325 with nothing between them to carry the eye across. */}
        <span className="grid min-w-0 flex-1 grid-cols-[minmax(0,1fr)_auto_auto] items-center gap-x-3 gap-y-1 sm:grid-cols-[minmax(0,1fr)_5.5rem_5.5rem_4.5rem_5rem_6rem] sm:gap-y-0">
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
            {/* The star sits immediately RIGHT of the title, on every row. Riding at
                the end of the flag cluster, it landed behind `new`/`dirty`/`draft`,
                so the one mark the human typed in themselves moved with whatever
                else the row happened to carry. */}
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
  const tools = usage?.top_tools ?? [];
  const errors = usage?.top_errors ?? [];

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
            {tools.length > 0 && (
              <Meta
                label="Top tools"
                value={tools
                  .slice(0, 3)
                  .map((tool) => `${tool.name} ${tool.count}`)
                  .join(' · ')}
                title={tools.map((tool) => `${tool.name} ${tool.count}`).join(' · ')}
              />
            )}
            {/* Volume alone hides where a session actually struggled, so failed
                calls get their own labelled pair instead of disappearing into
                the TOP TOOLS totals. */}
            {errors.length > 0 && (
              <Meta
                label="Top errors"
                value={errors
                  .slice(0, 3)
                  .map((tool) => `${tool.name} ${tool.count}`)
                  .join(' · ')}
                title={`${usage.error_count ?? 0} failed tool calls · ${errors
                  .map((tool) => `${tool.name} ${tool.count}`)
                  .join(' · ')}`}
                tone="warn"
              />
            )}
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
// It reuses the grid's dim-key/strong-value grammar so the three facts read as
// labelled data instead of one faint unlabelled sentence.
function Meta({
  label,
  value,
  title,
  tone = 'default',
}: {
  label: string;
  value: string;
  title?: string;
  tone?: 'default' | 'warn';
}) {
  return (
    <div className="flex min-w-0 items-baseline gap-1.5" title={title}>
      <dt className="shrink-0 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
        {label}
      </dt>
      <dd
        className={`min-w-0 truncate font-mono text-meta font-bold ${
          tone === 'warn' ? 'text-warn-strong' : 'text-white'
        }`}
      >
        {value}
      </dd>
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
            <SectionHeader>
              {/* One line, because the header it stands in for is one line: a
                  skeleton two lines tall collapses to one the moment data lands. */}
              <HeaderTitle
                name={
                  <span className="flex items-baseline gap-2">
                    <SkeletonBar type="text-ui" width="w-28" baz="h-2.5" tone="bg-muted/40" />
                    <SkeletonBar type="text-chip" width="w-40" baz="h-1.5" tone="bg-muted/20" />
                  </span>
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
                  <span className="grid min-w-0 flex-1 grid-cols-[minmax(0,1fr)_auto_auto] items-center gap-x-3 gap-y-1 sm:grid-cols-[minmax(0,1fr)_5.5rem_5.5rem_4.5rem_5rem_6rem] sm:gap-y-0">
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
    session.last_active_at,
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

function projectLabel(session: Session): string {
  // NEVER `workspace.label` for a draft: that is the DRAFT's name, and using it
  // as the grouping key gave every draft its own bogus top-level "project".
  const named =
    session.project_name?.trim() ||
    (isDraftWorkspace(session) ? '' : session.workspace?.label?.trim());
  if (named) return homeifyPath(named);
  const root = projectPath(session);
  if (root) return root.split('/').pop() || homeifyPath(root);
  return 'No project';
}

function projectRoot(sessions: Session[]): string {
  return sessions.map(projectPath).find(Boolean) ?? '';
}

function statusLabel(session: Session): string {
  if (sessionIsLive(session)) return 'LIVE';
  if (session.status === 'suspended') return 'WAITING';
  return 'IDLE';
}

function statusTone(session: Session): string {
  if (sessionIsLive(session)) return 'text-ok';
  if (session.status === 'suspended') return 'text-warn-strong';
  return 'text-dialog-hint';
}

function statusDot(session: Session): string {
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
