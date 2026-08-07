import type { MenuPosition } from './anchored-menu';
import { hostOf } from './endpoints';
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
  /** Last load failure. Set means offline/unauthorized; the row degrades. */
  error: string | null;
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
    if (!existing) return { conn, sessions: null, error: null };
    // The connection object itself can change (label, token, alts) without the
    // rows changing; keep the rows, take the new connection.
    return existing.conn === conn ? existing : { ...existing, conn };
  });
}

/**
 * The machines a scope covers. `null` is the whole fleet; a scope pointing at a
 * machine that is no longer paired falls back to the fleet rather than showing
 * an empty screen.
 */
export function scopedMachines(machines: FleetMachine[], scope: string | null): FleetMachine[] {
  if (!scope) return machines;
  const one = machines.find((machine) => machineKey(machine.conn) === scope);
  return one ? [one] : machines;
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

/** True once every machine in scope has answered (or failed) at least once. */
export function isFleetLoaded(machines: FleetMachine[], scope: string | null): boolean {
  const inScope = scopedMachines(machines, scope);
  return inScope.length > 0 && inScope.every((machine) => machine.sessions !== null || !!machine.error);
}

/**
 * The screen is only "unreachable" when NOTHING answers. One dead machine among
 * several is a degraded row inside a working list, not an error page — that is
 * the whole point of pairing more than one.
 */
export function fleetError(machines: FleetMachine[]): string | null {
  if (machines.length === 0) return null;
  const failed = machines.filter((machine) => machine.error);
  if (failed.length !== machines.length) return null;
  return failed[0]?.error ?? null;
}

/**
 * The failure the CURRENT scope is showing. Scoping to one machine makes that
 * machine the whole world: when it is the only thing on screen and it is dead,
 * the list must say so instead of pretending the machine simply has no work.
 */
export function scopeError(machines: FleetMachine[], scope: string | null): string | null {
  return fleetError(scopedMachines(machines, scope));
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
 * Whether the scope strip is on screen — and therefore where the fleet's live
 * and unread counts are stated. The strip exists only when there is more than
 * one machine to choose between, and every chip in it carries its machine's
 * two numbers, so the header line and the machine headers stay quiet while it
 * is up. With a single machine paired there is no strip, which is why the
 * header line takes the counts back instead of the screen going silent.
 */
export function showsScopeStrip(machines: FleetMachine[]): boolean {
  return machines.length > 1;
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

/** A turn is running in this session right now. */
export function sessionIsLive(session: Session): boolean {
  return session.live ?? session.status === 'running';
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

function sessionMillis(session: Session): number {
  return dateMillis(session.modified_at ?? session.last_active_at ?? session.created_at);
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

/** Group sessions by working directory, never by their optional project name. */
export function groupByWorkDir(sessions: Session[]): Array<[string, Session[]]> {
  const groups = new Map<string, Session[]>();
  for (const session of sessions) {
    const key = projectPath(session);
    const group = groups.get(key) ?? [];
    group.push(session);
    groups.set(key, group);
  }
  return [...groups.entries()];
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
        when: session.modified_at ?? session.last_active_at ?? session.created_at ?? null,
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
