import type { MenuPosition } from './anchored-menu';
import { hostOf } from './endpoints';
import { homeifyPath } from './path';
import type { GatewayConn, Session } from './types';

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
  /** `null` until this machine's first list lands — a machine still loading. */
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
   * TRUE ONLY ONCE THIS GATEWAY HAS SPOKEN TO THIS DEVICE since the screen mounted.
   *
   * `sessions` can be non-null without a single byte from the machine — the rows may
   * be the cached list this device painted last time. `All` is a list of machines
   * that ARE THERE, so it needs the difference: a cached list is what to paint the
   * moment the machine answers, never a reason to give it a section first and take
   * it away when the probe finally fails.
   */
  answered: boolean;
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
 * WHICH MACHINE THE LIST IS SHOWING, from the machine the reader last named.
 *
 * The pick is a PREFERENCE, not the answer: naming a machine that is gone, or one
 * that has stopped answering, falls back to `All` rather than parking the reader on
 * an empty screen they did not ask for. A machine that is down is not a place to be —
 * its rows are stale, its verbs refuse, and the retry lives on its tile.
 *
 * A FLEET OF ONE ALWAYS RESOLVES TO ITS MACHINE, up or down: there is no `All` above
 * a single machine, and that machine's failure is then the whole screen.
 */
export function resolveScope(machines: FleetMachine[], pick: string | null): string | null {
  if (machines.length === 1) return machineKey(machines[0].conn);
  const one = machines.find((machine) => machineKey(machine.conn) === pick);
  return one && !one.error ? pick : SCOPE_ALL;
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
  return failed[0]?.error ?? null;
}

/**
 * Which gateway a "New session" tap creates on, or `null` when the user has to
 * be asked first. Scope answers it; so does owning exactly one machine — a solo
 * user must never be shown a chooser with one row in it.
 */
export function newSessionTarget(
  machines: FleetMachine[],
  scope: string | null,
): GatewayConn | null {
  const inScope = scopedMachines(machines, scope);
  return inScope.length === 1 ? inScope[0].conn : null;
}

/** The machines the chooser offers: reachable first, unreachable never. */
export function creatableMachines(machines: FleetMachine[]): FleetMachine[] {
  return machines.filter((machine) => !machine.error);
}

/** What the "New session" menu is asking right now. */
export interface StartAsk {
  /** The machine the session will be created on; `null` while WHICH is unanswered. */
  on: GatewayConn | null;
  /** That machine's live row — the parked-drafts read hangs off it. */
  machine: FleetMachine | null;
  /** The machines offered while `on` is still null. */
  choices: FleetMachine[];
}

/**
 * The menu asks at most two questions, in this order: WHICH machine (only when
 * the scope cannot name one), then which workspace. Picking a machine ANSWERS
 * the first question — it must never be mistaken for the whole order, because
 * the parked drafts of that machine are what the second question offers.
 */
export function startAsk(
  machines: FleetMachine[],
  scopeTarget: GatewayConn | null,
  chosen: GatewayConn | null,
): StartAsk {
  const on = chosen ?? scopeTarget;
  // A machine unpaired while its menu was open cannot answer for the session:
  // ask again rather than aiming the create at a gateway that is no longer here.
  const machine = on ? (machines.find((row) => machineKey(row.conn) === machineKey(on)) ?? null) : null;
  return { on: machine ? on : null, machine, choices: creatableMachines(machines) };
}

/** Per-machine tallies for its chip and its section header. */
export function machineCounts(
  machine: FleetMachine,
  isLive: (session: Session) => boolean,
  isUnread: (session: Session) => boolean,
): { sessions: number; live: number; unread: number } {
  const rows = machine.sessions ?? [];
  return {
    sessions: rows.length,
    live: rows.filter(isLive).length,
    unread: rows.filter(isUnread).length,
  };
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

/** A turn is running in this session right now. */
export function sessionIsLive(session: Session): boolean {
  return session.live ?? session.status === 'running';
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
 * Nothing has happened in this session yet: no name, no turns, nothing running.
 * "New session" creates the row BEFORE the first message exists, so every
 * abandoned tap leaves one of these behind and the list keeps them out.
 */
export function sessionIsEmpty(session: Session): boolean {
  return (
    !session.title?.trim() &&
    Number(session.turn_count ?? 0) === 0 &&
    !sessionIsLive(session)
  );
}

/** Bands of the list order, best first. Every row is in exactly one. */
const FAVORITE_BAND = 0;
const DIRTY_BAND = 1;
const REST_BAND = 2;

/**
 * Starred work FIRST, then unsent work, then the order the gateway sent.
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
 * Rows the list paints. An empty session earns its place only by holding unsent
 * work or a star: that keeps abandoned "New session" taps out of the list
 * without stranding what you typed — hiding those rows left no way back to the
 * words and no way to delete the session holding them — and a session you
 * starred yourself is never hidden for being quiet.
 */
export function sessionIsListed(
  session: Session,
  flags: { hasDraftMessage: boolean; isFavorite: boolean },
): boolean {
  return !sessionIsEmpty(session) || flags.hasDraftMessage || flags.isFavorite;
}

/**
 * What a group-header delete MEANS for the sessions under it.
 *
 * The list groups by LABEL, not by project id: a group is a real project row, a
 * bare workspace root nothing ever named, or the `No project` bucket. Only the
 * first can be deleted AS a project, and only when every row in it agrees on the
 * id — a mixed group would claim to delete one project while quietly taking
 * members of another with it. Everything else is a fan-out over sessions, and
 * the copy must not promise a project it cannot deliver.
 *
 * The ids come from ALL of the group's sessions, never from the painted rows:
 * `sessionIsListed` hides empty, draft-less sessions, and a delete that stops at
 * what you can see leaves the invisible ones behind.
 */
export type ProjectDelete =
  | { kind: 'project'; projectId: string; sessionIds: string[] }
  | { kind: 'sessions'; sessionIds: string[] };

export function projectDelete(sessions: Session[]): ProjectDelete {
  const sessionIds = sessions.map((session) => session.id);
  const ids = new Set(sessions.map((session) => session.project_id ?? ''));
  const projectId = ids.size === 1 ? [...ids][0] : '';
  return projectId
    ? { kind: 'project', projectId, sessionIds }
    : { kind: 'sessions', sessionIds };
}

/**
 * What the start menu's parked-drafts picker should read, and NOTHING about when.
 *
 * Drafts are repo-scoped and the gateway only lists them through a session living
 * in that repo, so the read is a (machine, probe session) pair. Modelling it as a
 * value is the point: `draftsReadKey` gives that pair a STRING identity, and the
 * screen depends on the identity instead of on the objects. A background poll
 * hands back a new `FleetMachine` and new `Session` rows for the very same read —
 * keyed on object identity the request was aborted and restarted forever, and the
 * menu sat on "Reading drafts..." on a phone, where `resize` re-anchors it too.
 *
 * `wait` is the other half: a machine whose first list has not landed knows
 * nothing yet, and must not be reported as a project with no drafts parked.
 */
export type DraftsRead =
  | { kind: 'wait' }
  | { kind: 'none' }
  | { kind: 'read'; conn: GatewayConn; sid: string; repo: string };

export function draftsRead(machine: FleetMachine | null, probe: Session | null): DraftsRead {
  if (!machine) return { kind: 'none' };
  // Still loading — but a machine that FAILED to load has answered: no drafts.
  if (machine.sessions === null && !machine.error) return { kind: 'wait' };
  const repo = probe ? projectPath(probe) : '';
  if (!probe || !repo) return { kind: 'none' };
  return { kind: 'read', conn: machine.conn, sid: probe.id, repo };
}

/**
 * Identity of a read: same key, same list — do not read it again, and paint what
 * that key already answered.
 *
 * Keyed on the machine and the REPO, never on the probe session: the session is
 * only the door the gateway opens a repo's drafts through. "New session" mints a
 * row in that same repo, which then becomes the probe — so a session-keyed read
 * changed identity on every create, threw away a list it already had, and sent the
 * picker back to "Reading drafts..." every single time the menu opened.
 */
export function draftsReadKey(read: DraftsRead): string {
  return read.kind === 'read' ? `${machineKey(read.conn)}\u0000${read.repo}` : read.kind;
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
 * `last_active_at` is deliberately not read. It is the gateway's TOUCH clock (any
 * event, a model switch, a daemon start re-stamping the whole fleet), so ranking by
 * it made merely opening a session the freshest thing on the list.
 */
function sessionMillis(session: Session): number {
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

/** When a PROJECT last moved: the newest of its sessions. */
function projectMillis(sessions: Session[]): number {
  return sessions.reduce((newest, session) => Math.max(newest, sessionMillis(session)), 0);
}

/**
 * ONE page of a project's history, cut from the rows the screen is PAINTING.
 *
 * The pager walks the list a reader can SEE, and that list belongs to this
 * client: `sessionIsListed` hides the empty taps, `sessionOrder` lifts starred
 * and unsent work above the gateway's own ranking, and a live query narrows it
 * again. `GET /v1/sessions?root=` knows none of that, so its window at the same
 * offset is a DIFFERENT list — on one machine the gateway counted 1034 sessions
 * in a project this list paints 763 of, which puts the gateway's last page 27
 * pages beyond the pager's. Cutting page 1 and the page COUNT locally while
 * asking the gateway for pages 2 and up is what made the last page paint its
 * three real rows (239px tall) and then swap them 119ms later for an unrelated
 * ten-row window (582px): one tap, two paints, and neither of them the list.
 * The fleet poll already drains every window of every machine, so every row is
 * here already — one list, one arithmetic, one paint.
 *
 * `page` is CLAMPED, so a list that shrank under the reader (a deletion, a
 * filter, a smaller step) never gets a frame with an empty band in it.
 */
export function projectPage(
  sessions: Session[],
  page: number,
  pageSize: number,
): { page: number; pageCount: number; rows: Session[] } {
  const size = Math.max(1, Math.floor(pageSize) || 1);
  const pageCount = Math.max(1, Math.ceil(sessions.length / size));
  const at = Math.min(Math.max(1, Math.floor(page) || 1), pageCount);
  return { page: at, pageCount, rows: sessions.slice((at - 1) * size, at * size) };
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
 * The project a machine is CURRENTLY in: the root of its most recently touched
 * session. "New session" needs no question because of this — the machine has been
 * somewhere, and that somewhere is the answer until the user switches it.
 *
 * `null` only for a machine that has never run a session (or has not loaded yet);
 * then the menu offers browsing instead of naming a project that does not exist.
 */
export function machineProject(machine: FleetMachine | null): MachineProject | null {
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

/**
 * The whole "New session" order, as ONE value.
 *
 * It asks at most three questions, in this order: WHICH machine (only when the
 * scope cannot name one), which workspace, and — for a fork — what to call the
 * draft. Every answer belongs to the order and to nothing else, so LEAVING it (a
 * tap outside, Escape, Cancel) forgets all of them and the next "New session" tap
 * starts again at the first unanswered question.
 *
 * That is the entire point of one value. The picked machine used to live in a
 * state of its own, which the name dialog's dismissal never cleared: the next tap
 * found the leftover, took it for an answer, and created a session on that machine
 * without asking anything at all.
 */
export type StartFlow =
  | { step: 'idle' }
  | { step: 'menu'; at: MenuPosition; on: GatewayConn | null }
  /** The draft question, reached from the menu — a second verb, never the first. */
  | { step: 'drafts'; at: MenuPosition; on: GatewayConn }
  /** Browsing that machine's own files for the project to switch to. */
  | { step: 'browse'; at: MenuPosition; on: GatewayConn }
  | { step: 'name'; on: GatewayConn; clean: boolean };

/** No order in progress — and therefore no answers lying around. */
export const START_IDLE: StartFlow = { step: 'idle' };

/**
 * Open the menu, or RE-ANCHOR the open one: a resize is not an answer, so the
 * machine already picked survives it. No anchor left to hang from ends the order.
 */
export function startFlowOpen(flow: StartFlow, at: MenuPosition | null): StartFlow {
  if (!at) return START_IDLE;
  // A sub-question is still the same order: re-anchoring must not walk it back to
  // the verbs it was opened from.
  if (flow.step === 'drafts' || flow.step === 'browse') return { ...flow, at };
  return { step: 'menu', at, on: flow.step === 'menu' ? flow.on : null };
}

/** Answer WHICH machine — an answer inside the order, never the whole order. */
export function startFlowPick(flow: StartFlow, on: GatewayConn): StartFlow {
  return flow.step === 'menu' ? { ...flow, on } : flow;
}

/**
 * Take that answer back: the machine question again, with the order still open and
 * every other answer in it untouched.
 */
export function startFlowUnpick(flow: StartFlow): StartFlow {
  return flow.step === 'menu' ? { ...flow, on: null } : flow;
}

/** Hand the order to the name dialog, WITH the machine the fork happens on. */
export function startFlowName(on: GatewayConn, clean: boolean): StartFlow {
  return { step: 'name', on, clean };
}

/**
 * Walk the open menu to one of its own sub-questions, keeping the anchor it hangs
 * from: a step is still the SAME order, so leaving it forgets everything at once.
 */
export function startFlowStep(
  flow: StartFlow,
  step: 'drafts' | 'browse',
  on: GatewayConn,
): StartFlow {
  return flow.step === 'menu' || flow.step === 'drafts' || flow.step === 'browse'
    ? { step, at: flow.at, on }
    : flow;
}

/** Back out of a sub-question to the machine's own menu, without ending the order. */
export function startFlowBack(flow: StartFlow): StartFlow {
  return flow.step === 'drafts' || flow.step === 'browse'
    ? { step: 'menu', at: flow.at, on: flow.on }
    : flow;
}

/** The machine this order aims at so far, or `null` while nothing has answered. */
export function startFlowOn(flow: StartFlow): GatewayConn | null {
  return flow.step === 'idle' ? null : flow.on;
}
