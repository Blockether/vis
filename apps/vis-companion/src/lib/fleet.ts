import { hostOf } from './endpoints';
import { homeifyPath } from './path';
import type { GatewayConn, GatewayOverview, Session } from './types';

/**
 * Fleet rows belong to one paired machine; projects with the same path on different
 * machines remain distinct. Machine order follows pairing order.
 */
export interface FleetMachine {
  conn: GatewayConn;
  /**
   * The WINDOW this device holds of that machine's list — its newest rows, never the
   * whole of it (`GatewayClient.listSessions`) — and `null` until the first one lands.
   */
  sessions: Session[] | null;
  /** Last load failure. Set means offline/unauthorized; the row degrades. */
  error: string | null;
  /**
   * THE FAILURE ABOVE IS WHAT THIS DEVICE REMEMBERED, not what it measured in this run.
   *
   * A machine found dark is saved as dark (`lib/fleet-outage`), so a relaunch starts it
   * drained instead of meeting an hours-old corpse as a machine nobody has ever tried. That
   * memory drains the tile and the section exactly as a fresh failure does — but it is not
   * this device watching the fleet go dark, so it must not hand the whole screen to the
   * offline gate before one read of this run has been allowed to fail (see `fleetError`).
   */
  isRemembered?: boolean;
  /**
   * TRUE ONLY ONCE THIS GATEWAY HAS SPOKEN TO THIS DEVICE since the screen mounted.
   *
   * `sessions` can be non-null without a single byte from the machine — the rows may
   * be the cached list this device painted last time. `All` is a list of machines
   * that ARE THERE, so it needs the difference: a cached list is what to paint the
   * moment the machine answers, never a reason to give it a section first and take
   * it away when the probe finally fails.
   */
  answered: boolean;
  /**
   * WHAT THIS GATEWAY SAYS ITS PROJECTS ARE — `GET /v1/projects/overview`, the
   * counts tallied by the process that holds the sessions.
   *
   * `null` until this machine has answered one (a cached overview is seeded on
   * mount, so a machine returned to paints its header row in the first frame).
   * The numbers are NOT derived from `sessions`: deriving them meant a project
   * header could not be drawn before the whole fleet had been downloaded and
   * re-tallied, so switching gateways repainted the projects and then their
   * counts, page by page.
   */
  overview?: GatewayOverview | null;
  /**
   * The rows of the answer that carried `overview` which the gateway served as NEW: the
   * ones its `unread_count` includes. A visit mutes a held row's NEW long before the
   * gateway's read mark returns in a fresh overview, so this is how a header tells a row
   * the gateway still counts from one it has already let go (`readSinceCounted`).
   */
  countedUnread?: ReadonlySet<string>;
}

/** Identity of a machine in this screen: its transport URL. */
export function machineKey(conn: GatewayConn): string {
  return conn.url;
}

/**
 * Identity of ONE session across the fleet: the machine it lives on, then its
 * id. Two machines can hold the same id, so a row is named by both.
 */
export function sessionRowKey(conn: GatewayConn, sid: string): string {
  return `${machineKey(conn)}\u0000${sid}`;
}

/** What the chip and the machine header say. */
export function machineLabel(conn: GatewayConn): string {
  return conn.label?.trim() || hostOf(conn.url);
}

/**
 * Carry loaded machines across a re-pairing: entries that survive keep their
 * identity (and their rows), machines that were removed drop out, and newly
 * paired ones arrive blank. Identity matters — the list memoizes on it.
 */
export function reconcileMachines(conns: GatewayConn[], previous: FleetMachine[]): FleetMachine[] {
  const byKey = new Map(previous.map((machine) => [machineKey(machine.conn), machine]));
  return conns.map((conn) => {
    const existing = byKey.get(machineKey(conn));
    if (!existing) return { conn, sessions: null, error: null, answered: false };
    // The connection object itself can change (label, token, alts) without the
    // rows changing; keep the rows, take the new connection.
    return existing.conn === conn ? existing : { ...existing, conn };
  });
}

/**
 * THE WHOLE FLEET, and the only scope where more than one machine is on screen.
 *
 * Scoped to a single gateway, a fleet of six paints ONE hue: the palette, the rails
 * and the sections exist to tell machines apart, and a list that shows one machine at
 * a time can never do it. `All` stacks a named section per machine, each under its own
 * hue, so this is an ANSWER the switcher gives and not merely the unset state. A fleet
 * of one never offers it — the same list under a second name is not a choice.
 */
export const SCOPE_ALL = null;

/**
 * The machines a scope covers.
 *
 * `SCOPE_ALL` IS EVERY MACHINE THAT HAS ANSWERED. `All` exists to show the fleet as
 * separate machines — a section, a hue and a rail each — and a machine that is not
 * answering has no rows, no counts and no verbs to put in one; it was painting a
 * named band whose whole content was its own failure, in the middle of a list of
 * working computers. Its tile in the switcher keeps it visible and offers the retry
 * (see `MachineTab`), which is the only thing that machine can still do.
 *
 * A MACHINE WALKS IN WHEN IT SPEAKS, NEVER BEFORE. Cached rows made an untried
 * machine look alive: it took its section on the first frame and lost it a few
 * seconds later when the probe timed out, so every open of the list flashed a
 * gateway that had been asleep for days. While anything is still being tried and
 * nothing has answered yet, this is EMPTY — the screen is loading, not empty.
 *
 * A TOTAL BLACKOUT KEEPS EVERY MACHINE, because a screen with nothing on it cannot
 * say what happened: with nothing answering, the failures ARE the list (see
 * `fleetError`).
 *
 * A scope pointing at a machine that is no longer paired falls back to the fleet
 * rather than showing an empty screen.
 */
export function scopedMachines(machines: FleetMachine[], scope: string | null): FleetMachine[] {
  if (!scope) {
    const answering = machines.filter((machine) => !machine.error && machine.answered);
    if (answering.length > 0) return answering;
    // Nothing has spoken yet: waiting is not a blackout, and only a fleet that has
    // run out of machines to try hands the screen its failures.
    return machines.some((machine) => !machine.error) ? [] : machines;
  }
  const one = machines.find((machine) => machineKey(machine.conn) === scope);
  return one ? [one] : machines;
}

/**
 * WHICH SINGLE MACHINE THE LIST IS SHOWING.
 *
 * The reader's healthy picked machine wins. Otherwise the first machine that can answer
 * becomes active; if the whole fleet is dark, the first paired machine remains the scope
 * so the existing offline gate can explain the failure. `null` is only the initial sentinel
 * before machines hydrate, never a fleet-view answer.
 */
export function resolveScope(machines: FleetMachine[], pick: string | null): string | null {
  const picked = machines.find((machine) => machineKey(machine.conn) === pick);
  if (picked && !picked.error) return machineKey(picked.conn);
  const fallback = machines.find((machine) => !machine.error) ?? machines[0];
  return fallback ? machineKey(fallback.conn) : SCOPE_ALL;
}

/** Every session in scope, machine order preserved. */
export function scopedSessions(machines: FleetMachine[], scope: string | null): Session[] {
  return scopedMachines(machines, scope).flatMap((machine) => machine.sessions ?? []);
}

/**
 * The same narrowing over bare connections, for callers that hold the paired
 * list rather than the loaded fleet (searching, polling).
 */
export function scopedConns(conns: GatewayConn[], scope: string | null): GatewayConn[] {
  if (!scope) return conns;
  const one = conns.find((conn) => machineKey(conn) === scope);
  return one ? [one] : conns;
}

/**
 * WHO A FLEET SEARCH IS PUT TO — and who is answered for without being asked.
 *
 * A search costs the machine a ranked full-text scan and costs the reader the wait, so
 * the question only goes to gateways that can still answer one. TWO KINDS OF DEAD are
 * skipped, and both still count as ASKED, because a fleet that quietly shrinks to the
 * machines that work reports a search as complete that never read half the fleet:
 *
 *   - a machine whose LIST read failed (`error`): already drained out of `All`, not on
 *     screen, its tile carrying the retry — asking it put a machine the reader cannot
 *     even see in front of the progress they are watching;
 *   - a machine that was asked an EARLIER search and never answered it (`silent`).
 *     Having learned a gateway is dark, putting the same question to it on the next
 *     keystroke is how one asleep laptop turns every search into another timeout.
 *
 * `silent` is a MEMORY, NOT A VERDICT: the caller drops a machine from it the moment
 * any list read of that machine lands, so a gateway that was merely busy is searched
 * again within one poll and only a machine that keeps failing keeps being skipped.
 */
export interface SearchFanout {
  /** The machines the query is actually put to. */
  ask: GatewayConn[];
  /** Every machine the search covers, dark ones included — the count on screen. */
  asked: string[];
  /** Machines answered as unreachable without spending a request. */
  dark: string[];
}

export function searchFanout(
  conns: GatewayConn[],
  machines: FleetMachine[],
  scope: string | null,
  silent: ReadonlySet<string>,
): SearchFanout {
  const failed = new Set(
    machines.filter((machine) => machine.error !== null).map((machine) => machineKey(machine.conn)),
  );
  const targets = scopedConns(conns, scope);
  const isDark = (conn: GatewayConn) =>
    failed.has(machineKey(conn)) || silent.has(machineKey(conn));
  return {
    ask: targets.filter((conn) => !isDark(conn)),
    asked: targets.map(machineKey),
    dark: targets.filter(isDark).map(machineKey),
  };
}

/**
 * How far this run has got with a machine's list, and the only loading question the
 * screen asks about one. The footer, the skeleton, the machine tile and a machine's
 * own section all read this value rather than each deriving a loading state of its own
 * from `sessions`, `answered` and `error`, which is how they came to disagree.
 *
 * The rule, once: rows are paint, an answer is the gateway speaking in this run.
 * `sessions` turns non-null the moment the window this device saved is seeded on mount,
 * so a warm start paints rows that may be hours old while the machine is still
 * `reading` — that first paint is the point of saving them, and it is why the question
 * cannot be asked of `sessions`. A remembered outage is not an answer either: it is what
 * the previous run wrote down and kept for up to thirty days, and `load` already has a
 * read of that machine in flight behind it — painting it as a failure meant a laptop woken
 * an hour ago opened the app wearing `Reconnect` before anything had been asked, which is
 * why its tile says `Connecting…`. A machine whose read failed in this run has answered
 * the only way it can, so it is `down` and nothing is waiting on it.
 */
export type MachineRead = 'reading' | 'settled' | 'down';

export function machineRead(machine: FleetMachine): MachineRead {
  if (machine.error) return machine.isRemembered ? 'reading' : 'down';
  return machine.answered ? 'settled' : 'reading';
}

/**
 * The same question about a whole scope: `reading` while any machine in it still is,
 * `down` when every machine in it is, `settled` otherwise. One slow machine therefore
 * keeps the scope reading without keeping the machines beside it off the screen — the
 * rows are a separate question (see `MachineRead`).
 *
 * An empty scope is `reading`, not `settled`: `All` holds only the machines that have
 * spoken (see `scopedMachines`), so it stands empty exactly while the fleet is still
 * being tried. For that reason `All` weighs every paired machine here, not the drained
 * list — a fleet of six is not settled because the first of them answered.
 */
export function fleetRead(machines: FleetMachine[], scope: string | null): MachineRead {
  const inScope = scope ? scopedMachines(machines, scope) : machines;
  if (inScope.length === 0) return 'reading';
  const reads = inScope.map(machineRead);
  if (reads.includes('reading')) return 'reading';
  return reads.every((read) => read === 'down') ? 'down' : 'settled';
}

/**
 * The screen is only "unreachable" when NOTHING answers, and then it belongs to the
 * shell's offline gate rather than to this list. One dead machine among several is
 * simply not in the fleet view (see `scopedMachines`) — its tile keeps it visible and
 * carries the retry — which is the whole point of pairing more than one.
 */
export function fleetError(machines: FleetMachine[]): string | null {
  if (machines.length === 0) return null;
  const failed = machines.filter((machine) => machine.error);
  if (failed.length !== machines.length) return null;
  // A fleet still holding a machine whose darkness is only REMEMBERED has not run out of
  // machines to try: that read is in flight, and a saved verdict handing the shell its
  // offline screen would park every cold start on it — including the launch where the
  // laptop had been woken up an hour ago.
  if (failed.some((machine) => machine.isRemembered)) return null;
  return failed[0]?.error ?? null;
}

/**
 * Per-machine tallies for its chip and its section header.
 *
 * WHAT IT HOLDS is the gateway's own count, never a count of the rows this device
 * paged in: the list is a window, so counting it read low and moved as pages landed.
 * NEW is the gateway's count for the same reason. A Council wake lifts the sessions it
 * runs to the top of the list, and a count taken over the window lost every unread
 * conversation the wake pushed out of it. Rows read here since the gateway counted
 * them come off until its read mark returns.
 */
export function machineCounts(
  machine: FleetMachine,
  isLive: (session: Session) => boolean,
  isUnread: (session: Session) => boolean,
  isReadSince: (session: Session) => boolean = () => false,
): { sessions: number; live: number; unread: number } {
  const rows = machine.sessions ?? [];
  const counted = machine.overview
    ? { sessions: machine.overview.session_count, live: machine.overview.live_count }
    : { sessions: rows.length, live: rows.filter(isLive).length };
  return {
    ...counted,
    unread: unreadTally(machine.overview?.unread_count, rows, isUnread, isReadSince),
  };
}

/**
 * The rows a session-list answer served as NEW, which is the verdict the overview
 * beside them counted. `previous` is returned when nothing changed, so an unchanged
 * answer keeps the identity every memo hangs off.
 */
export function servedUnread(
  rows: Session[],
  previous?: ReadonlySet<string>,
): ReadonlySet<string> {
  const ids = new Set(rows.filter((row) => row.is_unread === true).map((row) => row.id));
  const isSame =
    previous !== undefined && previous.size === ids.size && [...ids].every((id) => previous.has(id));
  return isSame ? previous : ids;
}

/**
 * The rows of a machine's window the gateway COUNTED as new that this device has read
 * since, as `isSeen` decides. Taking off exactly those keeps a header level with the
 * rows under it after a visit, and never takes a visit off twice once the gateway's
 * own count has caught up.
 */
export function readSinceCounted(
  machine: FleetMachine,
  isSeen: (session: Session) => boolean,
): (session: Session) => boolean {
  const counted = machine.countedUnread;
  if (!counted || counted.size === 0) return () => false;
  return (session) => counted.has(session.id) && isSeen(session);
}

/** The gateway's NEW less what was read here since; the rows' own when it counted none. */
function unreadTally(
  counted: number | undefined,
  sessions: Session[],
  isUnread: (session: Session) => boolean,
  isReadSince: (session: Session) => boolean,
): number {
  if (typeof counted !== 'number') return sessions.filter(isUnread).length;
  return Math.max(0, counted - sessions.filter(isReadSince).length);
}

/**
 * What the live filter matched, and on how many machines. A search spans every
 * machine in scope, so the header has to be able to SAY so: "12 matches across
 * 2 of 3 machines" is the only proof the query left this gateway. Machines with
 * no hit still count as searched — they just contributed nothing.
 */
export function searchTally(filtered: { machine: FleetMachine; sessions: Session[] }[]): {
  matches: number;
  machines: number;
} {
  let matches = 0;
  let machines = 0;
  for (const entry of filtered) {
    matches += entry.sessions.length;
    if (entry.sessions.length > 0) machines += 1;
  }
  return { matches, machines };
}

/** Read the gateway's canonical liveness verdict. */
export function sessionIsLive(session: Session): boolean {
  return session.live;
}

/**
 * Whether the human has put this session away.
 *
 * The GATEWAY's stamp is the only copy of that decision, so a row painted here and the
 * same row on another device cannot disagree about it. An archived session is still
 * readable — it is the taking of new work that stops.
 */
export function sessionIsArchived(session: Session): boolean {
  const at = session.archived_at;
  return typeof at === 'number' && Number.isFinite(at);
}

/**
 * The run is parked on a human-input request nobody has answered yet.
 *
 * The one state the reader cannot infer from the row: the session is LIVE and
 * silent, and it will stay that way until they answer it themselves.
 */
export function sessionNeedsInput(session: Session): boolean {
  return session.is_awaiting_input === true;
}

/**
 * HOW MANY unanswered requests the session is parked on — 0 when none.
 *
 * The flag alone cannot say that answering one of two moved anything, which is
 * exactly the report: the badge stayed lit and nothing named the second request.
 * Older gateways send no count, so a parked row without one still counts as 1.
 */
export function sessionInputCount(session: Session): number {
  const open = session.awaiting_input_count;
  if (typeof open === 'number' && Number.isFinite(open)) return Math.max(0, Math.trunc(open));
  return sessionNeedsInput(session) ? 1 : 0;
}

/** Did the newest turn end early? The gateway owns both persisted verdicts. */
export function sessionWasStopped(session: Session): boolean {
  return session.was_interrupted === true || session.was_failed === true;
}

/** Newest conversation activity or explicit opening first, with a stable id tie-break. */
export function sessionOrder(sessions: Session[]): Session[] {
  const ordered = [...sessions].sort((a, b) => {
    const recency = sessionMillis(b) - sessionMillis(a);
    if (recency !== 0) return recency;
    return a.id === b.id ? 0 : a.id < b.id ? -1 : 1;
  });
  return ordered.every((row, index) => row === sessions[index]) ? sessions : ordered;
}

function dateMillis(value?: string | number): number {
  if (!value) return 0;
  const millis = new Date(value).getTime();
  return Number.isFinite(millis) ? millis : 0;
}

/** Last conversation activity or explicit opening, never a background registry touch. */
export function sessionMillis(session: Session): number {
  return Math.max(
    dateMillis(session.modified_at ?? session.created_at),
    dateMillis(session.last_opened_at ?? undefined),
  );
}

// Building an `Intl` formatter resolves locale data, which costs far more than
// formatting with one; a list labels every row on each render, so each shape is
// built once and reused.
let relativeFormat: Intl.RelativeTimeFormat | undefined;
const stampFormats = new Map<boolean, Intl.DateTimeFormat>();

function stampFormat(sameYear: boolean): Intl.DateTimeFormat {
  let format = stampFormats.get(sameYear);
  if (!format) {
    format = new Intl.DateTimeFormat(undefined, {
      month: 'short',
      day: 'numeric',
      ...(sameYear ? {} : { year: 'numeric' }),
      hour: '2-digit',
      minute: '2-digit',
    });
    stampFormats.set(sameYear, format);
  }
  return format;
}

/**
 * When a row last moved, as a human reads it: relative inside the last day ("3
 * hours ago"), an absolute DATE and time beyond it, with the year only when it is
 * not this one. A bare "5d" hides which day it was, and the exact stamp used to
 * live in a `title` tooltip — invisible on a touch screen.
 */
export function timeLabel(value?: string | number, now: number = Date.now()): string {
  const millis = dateMillis(value);
  if (!millis) return '-';
  const seconds = Math.round((millis - now) / 1000);
  const absolute = Math.abs(seconds);
  relativeFormat ??= new Intl.RelativeTimeFormat(undefined, { numeric: 'auto' });
  if (absolute < 60) return relativeFormat.format(seconds, 'second');
  if (absolute < 3_600) return relativeFormat.format(Math.round(seconds / 60), 'minute');
  if (absolute < 86_400) return relativeFormat.format(Math.round(seconds / 3_600), 'hour');
  const date = new Date(millis);
  return stampFormat(date.getFullYear() === new Date(now).getFullYear()).format(date);
}

// A DRAFT is a per-session clone parked at ~/.vis/drafts/<repo>/<label>; it is a
// workspace of the session, never a project of its own. `is_draft` is the gateway
// fact (list rows carry it); the path shape is the fallback for a gateway older
// than the flag, so an out-of-date daemon does not resurrect the
// one-project-per-draft bug.
const DRAFT_ROOT = /(^|\/)\.vis\/drafts\//;

export function isDraftWorkspace(session: Session): boolean {
  const workspace = session.workspace;
  if (!workspace) return false;
  if (typeof workspace.is_draft === 'boolean') return workspace.is_draft;
  return DRAFT_ROOT.test(workspace.root ?? '');
}

/**
 * The path a session is grouped under: THE GATEWAY'S OWN RULE, which is its
 * workspace `repo_root` when it has one, else its `root`
 * (`gateway/state.clj session-project-root`).
 *
 * Every row follows that rule, drafts included. Branching on `is_draft` here
 * disagreed with the machine for an ordinary session opened in a SUBDIRECTORY of
 * its repository: the gateway counts it under the repository, this device filed
 * it under the subdirectory, and the project got a second header holding the
 * rows the first one was still counting.
 */
export function projectPath(session: Session): string {
  const workspace = session.workspace;
  if (!workspace) return '';
  const path = workspace.repo_root || workspace.root;
  return path?.replace(/\/+$/, '') || '';
}

/**
 * The name a project header wears: the name its sessions AGREE on, else its folder name.
 *
 * A group is a working directory, and the rows in it are free to disagree about the optional
 * `project_name` the gateway carried — so reading the name off `sessions[0]` made the header
 * rename itself every time the row order moved (a turn starting, a star, an unsent draft). A name
 * only one row claims is not the project's name; the folder is, and it never moves. Same unanimity
 * rule `projectDelete` uses before it dares call a group a project.
 *
 * NEVER `workspace.label` for a draft: that is the DRAFT's name, and using it gave every draft its
 * own bogus top-level project.
 */
export function projectLabel(sessions: Session[]): string {
  const named = new Set(
    sessions.map(
      (session) =>
        session.project_name?.trim() ||
        (isDraftWorkspace(session) ? '' : session.workspace?.label?.trim()) ||
        '',
    ),
  );
  const agreed = named.size === 1 ? [...named][0] : '';
  const root = sessions.map(projectPath).find(Boolean) ?? '';
  return projectName(agreed, root);
}

/** Group by workspace root, preserving session order inside each group.
 * Roots use stable canonical root order, never activity.
 */
export function groupByWorkDir(sessions: Session[]): Array<[string, Session[]]> {
  return groupByRoot(sessions, projectPath);
}

/** `groupByWorkDir` over a caller's own choice of root for each row. */
function groupByRoot(
  sessions: Session[],
  rootOf: (session: Session) => string,
): Array<[string, Session[]]> {
  const groups = new Map<string, Session[]>();
  for (const session of sessions) {
    const key = rootOf(session);
    const group = groups.get(key) ?? [];
    group.push(session);
    groups.set(key, group);
  }
  return [...groups.entries()].sort(([leftRoot], [rightRoot]) =>
    leftRoot < rightRoot ? -1 : leftRoot > rightRoot ? 1 : 0,
  );
}

/**
 * Do two overviews say the SAME thing? — the clock sample the gateway stamps on
 * every answer excluded.
 *
 * An answer that changed nothing must not become a new fleet array: every memo
 * the list is built from hangs off that array, so re-patching an identical
 * overview re-ran the grouping and the sort under a reader who was only reading
 * (the same rule the session poll keeps).
 */
export function sameOverview(left: GatewayOverview, right: GatewayOverview): boolean {
  const stable = ({ server_time_ms: _clock, ...rest }: GatewayOverview) => JSON.stringify(rest);
  return stable(left) === stable(right);
}

/** What a header says a project holds and which actionable states are present. */
export interface Tally {
  count: number;
  live: number;
  /** Live sessions parked on human input. */
  awaiting?: number;
  /** Conversations holding an answer the reader has not seen yet. */
  unread?: number;
}

/**
 * The counts a MACHINE band wears, from the gateway when it has answered one.
 *
 * The rows on screen are a WINDOW — this device holds the pages it has walked, never
 * the fleet — so counting them said `12` under a machine of 400, and said a different
 * number at every stage of the walk. The gateway tallies its own store once
 * (`/v1/projects/overview`); summing the groups survives only as the answer for a
 * machine that has not spoken yet, and for a FILTERED list, where the honest count is
 * what is shown.
 */
export function machineTally(
  overview: GatewayOverview | null | undefined,
  groups: ProjectGroupView[],
): Tally {
  if (overview && typeof overview.session_count === 'number')
    return { count: overview.session_count, live: overview.live_count ?? 0 };
  return groups.reduce(
    (total, group) => ({
      count: total.count + group.tally.count,
      live: total.live + group.tally.live,
    }),
    { count: 0, live: 0 },
  );
}

/** A group with no rows of its own yet — a stable identity so a memo can bail out. */
const NO_SESSIONS: Session[] = [];

/**
 * ONE PROJECT AS THE LIST RENDERS IT: the gateway's own project row, plus whatever
 * rows of it this device is holding.
 *
 * A group used to be a bucket of downloaded rows — the list drained every session of
 * every machine and grouped what came back — so a project did not exist until its
 * rows had landed, and its header counted the window instead of the project. The
 * gateway tallies its own store (`/v1/projects/overview`) and cuts every page of it
 * (`GatewayClient.listProjectPage`); the rows here are only what a group can paint
 * before its own page answers.
 */
export interface ProjectGroupView {
  /** Canonical workspace root — the group's identity, and what a page is asked for by. */
  root: string;
  /** The name the header wears. */
  label: string;
  /** The gateway's project id when this root is a saved project, `''` otherwise. */
  projectId: string;
  /** What the project HOLDS, as whoever owns the list counted it. */
  tally: Tally;
  /** The rows of the machine's window that fall in this project, in the machine's order. */
  sessions: Session[];
}

/**
 * A machine's projects in canonical root order, matching the gateway contract.
 * Apply it to cached overviews and local-only roots too: response order, activity
 * and a root gaining an overview must not move existing headers.
 * The gateway owns counts, NEW included; roots absent from its overview use the rows
 * on screen. `isReadSince` names the rows read here since the gateway counted them
 * (`readSinceCounted`).
 */
export function projectGroups(
  overview: GatewayOverview | null | undefined,
  rows: Session[],
  isUnread: (session: Session) => boolean = () => false,
  isReadSince: (session: Session) => boolean = () => false,
): ProjectGroupView[] {
  const tallied = overview?.projects ?? [];
  // A row NAMES its project with `project_id`; its path is only how a client
  // guesses when nothing named it. So a row that claims a counted project is
  // filed under that project's root whatever its own path says — otherwise a row
  // whose path and project disagree minted a second header for a project the
  // overview was already counting it in.
  const rootById = new Map<string, string>();
  for (const project of tallied) {
    if (project.project_id) rootById.set(project.project_id, project.root);
  }
  const held = groupByRoot(
    rows,
    (session) =>
      (session.project_id ? rootById.get(session.project_id) : undefined) ?? projectPath(session),
  );
  const byRoot = new Map(held);
  const groups: ProjectGroupView[] = tallied.map((project) => {
    const sessions = byRoot.get(project.root) ?? NO_SESSIONS;
    return {
      root: project.root,
      label: projectName(project.name, project.root),
      projectId: project.project_id ?? '',
      tally: {
        count: project.session_count,
        live: project.live_count ?? 0,
        awaiting: project.awaiting_count ?? 0,
        unread: unreadTally(project.unread_count, sessions, isUnread, isReadSince),
      },
      sessions,
    };
  });
  const counted = new Set(tallied.map((project) => project.root));
  // A name a COUNTED project already wears is that project's name. A leftover
  // root whose rows carry it would print the same header twice — same word, two
  // places — so such a group is named after its own folder instead. Only groups
  // minted here are guarded: `searchGroups` has no overview to disagree with, and
  // its rows' `project_name` is the only name it will ever have.
  const takenLabels = new Set(groups.map((group) => group.label));
  return groups
    .concat(
      held
        .filter(([root]) => !counted.has(root))
        .map(([root, sessions]) => {
          const group = localGroup(root, sessions, isUnread);
          return takenLabels.has(group.label) ? { ...group, label: rootLabel(root) } : group;
        }),
    )
    .sort((left, right) => (left.root < right.root ? -1 : left.root > right.root ? 1 : 0));
}

/**
 * The same shape for a SEARCH — the one answer this device holds COMPLETE, since the
 * fanout narrows a list it was given. What is on screen is then the honest count.
 */
export function searchGroups(
  rows: Session[],
  isUnread: (session: Session) => boolean = () => false,
): ProjectGroupView[] {
  return groupByWorkDir(rows).map(([root, sessions]) => localGroup(root, sessions, isUnread));
}

/** A group nobody else counted: its own rows are the whole of it. */
function localGroup(
  root: string,
  sessions: Session[],
  isUnread: (session: Session) => boolean,
): ProjectGroupView {
  return {
    root,
    label: projectLabel(sessions),
    projectId: agreedProjectId(sessions),
    tally: {
      count: sessions.length,
      live: sessions.filter(sessionIsLive).length,
      awaiting: sessions.filter(sessionNeedsInput).length,
      unread: sessions.filter(isUnread).length,
    },
    sessions,
  };
}

/**
 * The project id every row of a group AGREES on, or `''`.
 *
 * Only a real project row can be deleted as a project, and only when the group is
 * unanimous — a mixed group would claim to delete one project while quietly taking
 * members of another with it.
 */
function agreedProjectId(sessions: Session[]): string {
  const ids = new Set(sessions.map((session) => session.project_id ?? ''));
  return ids.size === 1 ? ([...ids][0] ?? '') : '';
}

/** The name a bare root wears when nothing named the project: its last segment. */
function rootLabel(root: string): string {
  if (!root) return 'No project';
  return root.replace(/[\\/]+$/, '').split(/[\\/]/).pop() || homeifyPath(root);
}

/**
 * A project's name, never its path. An older gateway stored the folder path as the
 * name, so a name that is a path wears only its last folder name.
 */
function projectName(name: string, root: string): string {
  const named = name.trim();
  if (!named) return rootLabel(root);
  return /^(~|\/|[A-Za-z]:[\\/])/.test(named) ? rootLabel(named) : named;
}

/** Where a machine is working right now, as the menu says it out loud. */
export interface MachineProject {
  /** The repo root a new session starts in. */
  path: string;
  /** Its last segment — the name a human uses for the project. */
  label: string;
  /** When that project last moved, or `null` when nothing is recorded. */
  when: string | null;
}

/**
 * The project a machine is CURRENTLY in: the one its gateway stamped with the most
 * recent activity. "New session" needs no question because of this — the machine has
 * been somewhere, and that somewhere is the answer until the user switches it.
 *
 * Read from the gateway's own project tally, so it names a project this device may
 * never have paged a row of; the window it holds answers for a machine whose overview
 * has not landed yet.
 *
 * `null` only for a machine that has never run a session (or has not loaded yet); then
 * the menu offers browsing instead of naming a project that does not exist.
 */
export function machineProject(machine: FleetMachine | null): MachineProject | null {
  let counted: { root: string; whenMs: number } | null = null;
  for (const project of machine?.overview?.projects ?? []) {
    if (!project.root) continue;
    const whenMs = project.last_activity_ms ?? 0;
    if (!counted || whenMs > counted.whenMs) counted = { root: project.root, whenMs };
  }
  if (counted)
    return {
      path: counted.root,
      label: counted.root.split('/').filter(Boolean).pop() ?? counted.root,
      when: counted.whenMs > 0 ? new Date(counted.whenMs).toISOString() : null,
    };
  const sessions = machine?.sessions ?? [];
  let best: { path: string; whenMs: number; when: string | null } | null = null;
  for (const session of sessions) {
    const path = projectPath(session);
    if (!path) continue;
    const whenMs = sessionMillis(session);
    if (!best || whenMs > best.whenMs)
      best = {
        path,
        whenMs,
        when: session.modified_at ?? session.created_at ?? null,
      };
  }
  if (!best) return null;
  return {
    path: best.path,
    label: best.path.split('/').filter(Boolean).pop() ?? best.path,
    when: best.when,
  };
}
