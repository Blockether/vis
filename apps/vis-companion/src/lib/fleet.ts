import { hostOf } from './endpoints';
import { homeifyPath } from './path';
import type { GatewayConn, GatewayOverview, Session } from './types';

/**
 * The fleet model behind the sessions list.
 *
 * A machine OWNS its projects: every session on screen belongs to exactly one
 * paired gateway, and a project only exists inside the machine it lives on. Two
 * machines checked out of the same repo are two different projects — the folder
 * name is not an identity across machines, so it is never used to merge rows.
 *
 * The screen keeps one of these per paired connection and renders them in the
 * order the machines were paired; the scope chip narrows to one of them.
 */
export interface FleetMachine {
  conn: GatewayConn;
  /**
   * The WINDOW this device holds of that machine's list — its newest rows, never the
   * whole of it (`GatewayClient.listSessions`) — and `null` until the first one lands.
   */
  sessions: Session[] | null;
  /**
   * This machine's sessions PARKED on an unanswered human-input request, as the gateway
   * answered them BESIDE the window (`GatewayClient.parkedSessions`).
   *
   * The list is ordered by content time and nothing else, so a parked run sits wherever
   * it last spoke — in a long fleet, past the end of the window this device has read.
   * The demand is the one state a reader cannot infer and only they can clear, so it
   * travels complete and gets PINNED above the list; an ordering that lifted it into
   * the list moved every row under the reader the moment a turn asked or was answered.
   */
  awaiting?: Session[];
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
}

/** Identity of a machine in this screen: its transport URL. */
export function machineKey(conn: GatewayConn): string {
  return conn.url;
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
export function reconcileMachines(
  conns: GatewayConn[],
  previous: FleetMachine[],
): FleetMachine[] {
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
    machines
      .filter((machine) => machine.error !== null)
      .map((machine) => machineKey(machine.conn)),
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

/** True once every machine in scope has answered (or failed) at least once. */
export function isFleetLoaded(machines: FleetMachine[], scope: string | null): boolean {
  const inScope = scope ? scopedMachines(machines, scope) : machines;
  return inScope.length > 0 && inScope.every((machine) => machine.sessions !== null || !!machine.error);
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
 * Unread is the one number that stays local — it is this device's reading of the
 * window it holds, and an answer older than that window is not news any more.
 */
export function machineCounts(
  machine: FleetMachine,
  isLive: (session: Session) => boolean,
  isUnread: (session: Session) => boolean,
): { sessions: number; live: number; unread: number } {
  const rows = machine.sessions ?? [];
  const counted = machine.overview
    ? { sessions: machine.overview.session_count, live: machine.overview.live_count }
    : { sessions: rows.length, live: rows.filter(isLive).length };
  return { ...counted, unread: rows.filter(isUnread).length };
}

/**
 * What the live filter matched, and on how many machines. A search spans every
 * machine in scope, so the header has to be able to SAY so: "12 matches across
 * 2 of 3 machines" is the only proof the query left this gateway. Machines with
 * no hit still count as searched — they just contributed nothing.
 */
export function searchTally(
  filtered: { machine: FleetMachine; sessions: Session[] }[],
): { matches: number; machines: number } {
  let matches = 0;
  let machines = 0;
  for (const entry of filtered) {
    matches += entry.sessions.length;
    if (entry.sessions.length > 0) machines += 1;
  }
  return { matches, machines };
}

/**
 * Where a row the gateway could not place sits: after every placed one.
 *
 * The ORDER is the SERVER's answer, decided once for every client: running
 * sessions first, then the freshest first — the same order the gateway lists
 * sessions in, so a query narrows the list instead of reshuffling it. What is
 * left over is what no gateway can see: an unsent draft in this device's
 * composer, and the local metadata of a row the search endpoint did not return.
 * Those sort last rather than competing on a freshness this side would have to
 * guess at.
 */
export const SEARCH_UNPLACED = Number.MAX_SAFE_INTEGER;

/**
 * Order the rows a query matched by the PLACE the gateway gave them — its
 * position in the search answer, which is the gateway's own freshest-first
 * order.
 *
 * It used to sort by the relevance BAND instead (title hits, then the user's
 * words, then the assistant's), which buried this morning's session under
 * every year-old title that happened to contain the word: the dates jumped up
 * and down the list. Bands still travel on `SessionMatch.rank`, to say WHERE a
 * query hit; they no longer decide where a row sits.
 *
 * Bands the list paints itself (`sessionOrder`) are applied AFTER this, so a
 * starred row stays on top of its own search.
 */
export function searchOrder(
  sessions: Session[],
  placeOf: (session: Session) => number,
): Session[] {
  const rows = sessions.map((session, index) => ({
    session,
    index,
    place: placeOf(session),
  }));
  rows.sort((a, b) => (a.place !== b.place ? a.place - b.place : a.index - b.index));
  return rows.map((row) => row.session);
}

/** Read the gateway's canonical liveness verdict. */
export function sessionIsLive(session: Session): boolean {
  return session.live;
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

/** Bands of the list order, best first. Every row is in exactly one. */
const FAVORITE_BAND = 0;
const DIRTY_BAND = 1;
const REST_BAND = 2;

/**
 * Starred work FIRST, then unsent work, then the order the gateway sent.
 *
 * Only a SEARCH answer needs this now: the navigator list arrives already
 * banded from the gateway, which owns it. A search is a COMPLETE match set in
 * the gateway's own order, so re-banding it here is honest arithmetic.
 *
 * A star is the one piece of ordering the human typed in themselves, so it wins
 * outright: a favorite sits on top whether it is live, unread, or a year cold.
 *
 * A draft message lives on THIS device only: the fleet cannot remind you about
 * it from another machine, and the session holding it is usually empty, so it
 * sorts to the bottom of every timestamp order there is. So it goes above the
 * rest.
 *
 * The order is TOTAL and does not lean on `Array#sort` being stable: favorites
 * compare by the rank they were starred with, then by id, and everything else
 * by its incoming position. So the same sessions always paint in the same
 * order, however many stars there are, whatever order the gateway listed them
 * in, and whichever engine runs the sort.
 */
export function sessionOrder(
  sessions: Session[],
  rank: {
    favoriteRank: (session: Session) => number | null;
    hasDraftMessage: (session: Session) => boolean;
  },
): Session[] {
  const rows = sessions.map((session, index) => {
    const pin = rank.favoriteRank(session);
    const band = pin !== null
      ? FAVORITE_BAND
      : rank.hasDraftMessage(session)
        ? DIRTY_BAND
        : REST_BAND;
    return { session, index, band, pin: pin ?? 0 };
  });
  if (rows.every((row) => row.band === REST_BAND)) return sessions;
  rows.sort((a, b) => {
    if (a.band !== b.band) return a.band - b.band;
    if (a.band === FAVORITE_BAND) {
      if (a.pin !== b.pin) return a.pin - b.pin;
      if (a.session.id !== b.session.id) return a.session.id < b.session.id ? -1 : 1;
    }
    return a.index - b.index;
  });
  return rows.map((row) => row.session);
}



/**
 * The rows a QUERY may match on one machine: what is loaded, plus the sessions a
 * server-side transcript hit named that this machine has not paged in yet.
 *
 * The list is paged, so filtering the loaded window alone made search find only
 * what was already on screen — a hit in a session further down the fleet's own
 * ordering was silently intersected away. Hydrated rows are appended (newest
 * first among themselves); the gateway's order still owns everything it sent.
 */
export function withSearchHits(sessions: Session[], hits: Session[]): Session[] {
  if (hits.length === 0) return sessions;
  const known = new Set(sessions.map((session) => session.id));
  const extra = hits
    .filter((session) => !known.has(session.id))
    .sort((a, b) => sessionMillis(b) - sessionMillis(a));
  return extra.length === 0 ? sessions : [...sessions, ...extra];
}

function dateMillis(value?: string): number {
  if (!value) return 0;
  const millis = new Date(value).getTime();
  return Number.isFinite(millis) ? millis : 0;
}

/**
 * When a row last moved, for ORDERING: content time only.
 *
 * A TOUCH clock never reaches this ranking. The gateway keeps its own `:last-active`
 * stamp (any event, a model switch, a daemon start re-stamping the whole fleet) to
 * itself, because ranking by it made merely opening a session the freshest thing.
 */
export function sessionMillis(session: Session): number {
  return dateMillis(session.modified_at ?? session.created_at);
}

/**
 * When a row last moved, as a human reads it: relative inside the last day ("3
 * hours ago"), an absolute DATE and time beyond it, with the year only when it is
 * not this one. A bare "5d" hides which day it was, and the exact stamp used to
 * live in a `title` tooltip — invisible on a touch screen.
 */
export function timeLabel(value?: string, now: number = Date.now()): string {
  const millis = dateMillis(value);
  if (!millis) return '-';
  const seconds = Math.round((millis - now) / 1000);
  const absolute = Math.abs(seconds);
  const relative = new Intl.RelativeTimeFormat(undefined, { numeric: 'auto' });
  if (absolute < 60) return relative.format(seconds, 'second');
  if (absolute < 3_600) return relative.format(Math.round(seconds / 60), 'minute');
  if (absolute < 86_400) return relative.format(Math.round(seconds / 3_600), 'hour');
  const date = new Date(millis);
  const sameYear = date.getFullYear() === new Date(now).getFullYear();
  return new Intl.DateTimeFormat(undefined, {
    month: 'short',
    day: 'numeric',
    ...(sameYear ? {} : { year: 'numeric' }),
    hour: '2-digit',
    minute: '2-digit',
  }).format(date);
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
 * The path a session is grouped under: its working directory. Drafts use their
 * repository root so the draft clone does not become a second group.
 */
export function projectPath(session: Session): string {
  const workspace = session.workspace;
  if (!workspace) return '';
  const path = isDraftWorkspace(session)
    ? workspace.repo_root || workspace.root
    : workspace.root || workspace.repo_root;
  return path?.replace(/\/+$/, '') || '';
}

/**
 * The name a project header wears: the name its sessions AGREE on, else its folder.
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
  if (agreed) return homeifyPath(agreed);
  const root = sessions.map(projectPath).find(Boolean) ?? '';
  if (root) return root.split('/').pop() || homeifyPath(root);
  return 'No project';
}

/**
 * Group sessions by working directory, never by their optional project name, and put the groups in
 * an order the PROJECT owns.
 *
 * The order used to be whatever order the Map was filled in — that is, a project sat wherever its
 * best-ranked session sat. Nothing about a project decided where its header went: `sessionOrder`
 * lifts a starred or unsent row to the front of the machine's list and dragged its whole project
 * with it, the gateway floats every live session to the top of the fleet, and both are per-row
 * facts. So one turn starting moved a header the user was reading, a star saved on THIS device
 * reordered the screen against every other device, and the same data painted two ways.
 *
 * The key is the project's own: work RUNNING first (the gateway's own navigator rule, one level
 * up), then the project's most recent activity, then the workspace root. Total and data-only, so
 * every device paints identical projects in an identical order, and only a real change to a project
 * moves it. Stars and drafts still lift rows INSIDE the group, which is where a per-device
 * preference belongs.
 */
export function groupByWorkDir(sessions: Session[]): Array<[string, Session[]]> {
  const groups = new Map<string, Session[]>();
  for (const session of sessions) {
    const key = projectPath(session);
    const group = groups.get(key) ?? [];
    group.push(session);
    groups.set(key, group);
  }
  return [...groups.entries()].sort(([leftRoot, left], [rightRoot, right]) => {
    const live = Number(!left.some(sessionIsLive)) - Number(!right.some(sessionIsLive));
    if (live !== 0) return live;
    const recency = projectMillis(right) - projectMillis(left);
    if (recency !== 0) return recency;
    return leftRoot < rightRoot ? -1 : leftRoot > rightRoot ? 1 : 0;
  });
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
  /** Finished answers this device has not read yet. */
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
 * A machine's projects, in the order a PROJECT owns: work running first, then the
 * project's own last activity, then the root — the same key `groupByWorkDir` sorts
 * by, decided from the gateway's counts instead of from rows this device happens to
 * hold.
 *
 * A root the gateway did not tally — a draft workspace, or any root at all when the
 * machine has answered no overview yet — is not in its ordering either, so it follows
 * the projects that are, counted by what is on screen.
 */
export function projectGroups(
  overview: GatewayOverview | null | undefined,
  rows: Session[],
  isUnread: (session: Session) => boolean = () => false,
): ProjectGroupView[] {
  const held = groupByWorkDir(rows);
  const byRoot = new Map(held);
  const tallied = overview?.projects ?? [];
  const groups: ProjectGroupView[] = tallied
    .map((project) => {
      const sessions = byRoot.get(project.root) ?? NO_SESSIONS;
      return {
        root: project.root,
        label: project.name.trim() ? homeifyPath(project.name.trim()) : rootLabel(project.root),
        projectId: project.project_id ?? '',
        tally: {
          count: project.session_count,
          live: project.live_count ?? 0,
          awaiting: project.awaiting_count ?? 0,
          unread: sessions.filter(isUnread).length,
        },
        sessions,
        when: project.last_activity_ms ?? 0,
      };
    })
    .sort((left, right) => {
      const live = Number(left.tally.live === 0) - Number(right.tally.live === 0);
      if (live !== 0) return live;
      const recency = right.when - left.when;
      if (recency !== 0) return recency;
      return left.root < right.root ? -1 : left.root > right.root ? 1 : 0;
    })
    .map(({ when: _when, ...group }) => group);
  const counted = new Set(tallied.map((project) => project.root));
  return groups.concat(
    held
      .filter(([root]) => !counted.has(root))
      .map(([root, sessions]) => localGroup(root, sessions, isUnread)),
  );
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
  return root.split('/').pop() || homeifyPath(root);
}

/** When a PROJECT last moved: the newest of its sessions. */
function projectMillis(sessions: Session[]): number {
  return sessions.reduce((newest, session) => Math.max(newest, sessionMillis(session)), 0);
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
