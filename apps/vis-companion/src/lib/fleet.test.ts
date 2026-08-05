import { describe, expect, it } from 'vitest';
import {
  creatableMachines,
  pageSessions,
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
  scopedMachines,
  scopeError,
  scopedConns,
  searchTally,
  scopedSessions,
  sessionIsEmpty,
  sessionIsListed,
  sessionIsPeeked,
  sessionOrder,
  showsScopeStrip,
  startAsk,
  START_IDLE,
  startFlowName,
  startFlowOn,
  startFlowOpen,
  startFlowPick,
  type FleetMachine,
} from './fleet';
import type { GatewayConn, Session } from './types';

const studio: GatewayConn = { url: 'http://studio.local:7890', label: 'studio' };
const tower: GatewayConn = { url: 'http://tower.local:7890' };
const vps: GatewayConn = { url: 'http://10.0.0.5:7890', label: 'vps-eu' };

function session(id: string, extra: Partial<Session> = {}): Session {
  return { id, title: id, ...extra };
}

function machine(conn: GatewayConn, sessions: Session[] | null, error: string | null = null): FleetMachine {
  return { conn, sessions, error };
}

describe('machineLabel', () => {
  it('prefers the pairing label and falls back to the host', () => {
    expect(machineLabel(studio)).toBe('studio');
    expect(machineLabel(tower)).toBe('tower.local:7890');
    expect(machineLabel({ url: 'http://tower.local:7890', label: '   ' })).toBe('tower.local:7890');
  });
});

describe('reconcileMachines', () => {
  it('keeps loaded rows across a re-pair, drops removed machines, blanks new ones', () => {
    const loaded = machine(studio, [session('a')]);
    const next = reconcileMachines([studio, vps], [loaded, machine(tower, [session('b')])]);
    expect(next).toHaveLength(2);
    expect(next[0]).toBe(loaded);
    expect(next[1]).toEqual({ conn: vps, sessions: null, error: null });
  });

  it('takes a renamed connection without dropping its rows', () => {
    const renamed = { ...studio, label: 'desk' };
    const next = reconcileMachines([renamed], [machine(studio, [session('a')])]);
    expect(next[0].conn).toBe(renamed);
    expect(next[0].sessions).toEqual([session('a')]);
  });
});

describe('scope', () => {
  const machines = [machine(studio, [session('a'), session('b')]), machine(tower, [session('c')])];

  it('null scope is the whole fleet, in pairing order', () => {
    expect(scopedSessions(machines, null).map((s) => s.id)).toEqual(['a', 'b', 'c']);
  });

  it('a scope narrows to one machine', () => {
    expect(scopedMachines(machines, tower.url)).toEqual([machines[1]]);
    expect(scopedSessions(machines, tower.url).map((s) => s.id)).toEqual(['c']);
  });

  it('a scope on an unpaired machine falls back to the fleet', () => {
    expect(scopedSessions(machines, 'http://gone.local:7890')).toHaveLength(3);
  });

  it('is loaded only once every machine in scope has answered', () => {
    const half = [machine(studio, [session('a')]), machine(tower, null)];
    expect(isFleetLoaded(half, null)).toBe(false);
    expect(isFleetLoaded(half, studio.url)).toBe(true);
    expect(isFleetLoaded([machine(tower, null, 'offline')], null)).toBe(true);
    expect(isFleetLoaded([], null)).toBe(false);
  });
});

describe('fleetError', () => {
  it('stays silent while anything still answers', () => {
    expect(fleetError([machine(studio, [session('a')]), machine(tower, null, 'offline')])).toBeNull();
    expect(fleetError([machine(studio, null)])).toBeNull();
    expect(fleetError([])).toBeNull();
  });

  it('reports the first failure when every machine is down', () => {
    expect(fleetError([machine(studio, null, 'refused'), machine(tower, null, 'offline')])).toBe('refused');
  });
});

describe('newSessionTarget', () => {
  const fleet = [machine(studio, []), machine(tower, [])];

  it('asks when the fleet scope covers several machines', () => {
    expect(newSessionTarget(fleet, null)).toBeNull();
  });

  it('creates straight on the scoped machine', () => {
    expect(newSessionTarget(fleet, tower.url)).toBe(tower);
  });

  it('never asks a solo user', () => {
    expect(newSessionTarget([machine(studio, [])], null)).toBe(studio);
  });

  it('offers only reachable machines in the chooser', () => {
    const withDead = [...fleet, machine(vps, null, 'offline')];
    expect(creatableMachines(withDead).map((m) => m.conn)).toEqual([studio, tower]);
  });
});

// Regression: with several machines paired, the "New session" caret asked WHICH
// machine and treated the answer as the whole order — it created a trunk session
// on that machine right there. The workspace question never came, so the parked
// drafts were never listed and the user landed in the repo itself unasked.
describe('startAsk', () => {
  const fleet = [machine(studio, [session('a')]), machine(tower, [session('c')])];

  it('asks which machine while the scope names none, offering the reachable ones', () => {
    const ask = startAsk([...fleet, machine(vps, null, 'offline')], null, null);
    expect(ask.on).toBeNull();
    expect(ask.machine).toBeNull();
    expect(ask.choices.map((m) => m.conn)).toEqual([studio, tower]);
  });

  it('takes the scoped or solo machine without asking', () => {
    expect(startAsk(fleet, tower, null).on).toBe(tower);
    expect(startAsk(fleet, tower, null).machine).toBe(fleet[1]);
  });

  it('makes the picked machine the one the workspace question is about', () => {
    const ask = startAsk(fleet, null, tower);
    expect(ask.on).toBe(tower);
    expect(ask.machine).toBe(fleet[1]);
  });

  it('reads the parked drafts off the machine just picked, not "none"', () => {
    expect(draftsRead(startAsk(fleet, null, null).machine, null)).toEqual({ kind: 'none' });
    const ask = startAsk(fleet, null, tower);
    const probe = session('c', { workspace: { root: '/Users/me/vis' } });
    expect(draftsRead(ask.machine, probe)).toEqual({
      kind: 'read',
      conn: tower,
      sid: 'c',
      repo: '/Users/me/vis',
    });
  });

  it('a machine unpaired under the open menu cannot answer for the session', () => {
    expect(startAsk(fleet, null, vps).on).toBeNull();
  });
});

describe('machineCounts', () => {
  it('tallies sessions, live and unread for one machine', () => {
    const rows = [session('a', { live: true }), session('b'), session('c', { live: true })];
    const counts = machineCounts(
      machine(studio, rows),
      (s) => s.live === true,
      (s) => s.id === 'b',
    );
    expect(counts).toEqual({ sessions: 3, live: 2, unread: 1 });
  });

  it('a machine that has not answered counts as nothing', () => {
    expect(machineCounts(machine(tower, null), () => true, () => true)).toEqual({
      sessions: 0,
      live: 0,
      unread: 0,
    });
  });
});

describe('scopeError', () => {
  const fleet = [machine(studio, [session('a')]), machine(tower, null, 'offline')];

  it('stays null while anything in scope answers', () => {
    expect(scopeError(fleet, null)).toBeNull();
    expect(scopeError(fleet, machineKey(studio))).toBeNull();
  });

  it('surfaces the failure when the scope points at the dead machine', () => {
    expect(scopeError(fleet, machineKey(tower))).toBe('offline');
  });

  it('surfaces a total blackout for the unscoped fleet', () => {
    expect(scopeError([machine(studio, null, 'down'), machine(tower, null, 'offline')], null)).toBe('down');
  });
});

describe('search across the fleet', () => {
  it('an unscoped search targets every paired gateway, a scoped one targets that gateway', () => {
    const conns = [studio, tower, vps];
    expect(scopedConns(conns, null)).toEqual(conns);
    expect(scopedConns(conns, tower.url)).toEqual([tower]);
    // A scope left over from an unpaired machine must not silence the search.
    expect(scopedConns(conns, 'http://gone.local:7890')).toEqual(conns);
  });

  it('tallies the hits and the machines that produced them', () => {
    const filtered = [
      { machine: machine(studio, [session('a'), session('b')]), sessions: [session('a')] },
      { machine: machine(tower, [session('c')]), sessions: [] },
      { machine: machine(vps, [session('d')]), sessions: [session('d')] },
    ];
    expect(searchTally(filtered)).toEqual({ matches: 2, machines: 2 });
    expect(searchTally([])).toEqual({ matches: 0, machines: 0 });
  });
});

// A session with a composer full of unsent words is DIRTY, not empty: the list
// used to hide every untitled turn-less session, so tapping "New session",
// typing, and going back left the words in a row nobody could see, reopen, or
// delete. Dirty rows stay listed until they are sent or thrown away.
describe('dirty sessions', () => {
  const fresh = (extra: Partial<Session> = {}): Session => ({ id: 's1', title: '', ...extra });

  it('treats an untitled, turn-less, idle session as empty', () => {
    expect(sessionIsEmpty(fresh())).toBe(true);
    expect(sessionIsEmpty(fresh({ title: '  ' }))).toBe(true);
    expect(sessionIsEmpty(fresh({ turn_count: 0 }))).toBe(true);
  });

  it('never calls a named, used, or running session empty', () => {
    expect(sessionIsEmpty(fresh({ title: 'Fix the parser' }))).toBe(false);
    expect(sessionIsEmpty(fresh({ turn_count: 1 }))).toBe(false);
    expect(sessionIsEmpty(fresh({ status: 'running' }))).toBe(false);
    expect(sessionIsEmpty(fresh({ live: true }))).toBe(false);
  });

  it('lists a dirty session and keeps hiding the abandoned taps', () => {
    expect(sessionIsListed(fresh(), { hasDraftMessage: true, isFavorite: false })).toBe(true);
    expect(sessionIsListed(fresh(), { hasDraftMessage: false, isFavorite: false })).toBe(false);
    expect(sessionIsListed(fresh(), { hasDraftMessage: false, isFavorite: true })).toBe(true);
    expect(
      sessionIsListed(fresh({ title: 'Named' }), { hasDraftMessage: false, isFavorite: false }),
    ).toBe(true);
  });
});

// Regression (reported in-app: "we have this function which is hiding the session
// if it's longer then 1 hour and not touched … it SHOULD NEVER HIDE the RUNNING
// SESSIONS OR THE ONES WHICH ARE FINISHED AND NOT READ"). A collapsed project
// used to peek only its live rows plus whatever was touched within the last hour,
// so the one row that MUST be seen — an answer that landed while the app was shut,
// still wearing its unread badge, the very thing the push notification was about —
// disappeared an hour later, and a session merely waiting for human input went
// with it. Age may only ever hide a session that is idle, answered and read.
describe('sessionIsPeeked', () => {
  const HOUR = 60 * 60 * 1000;
  const now = 1_700_000_000_000;
  const aged = (ms: number, extra: Partial<Session> = {}): Session =>
    session('s1', { modified_at: new Date(now - ms).toISOString(), ...extra });
  const peek = (
    row: Session,
    flags: Partial<{ isUnread: boolean; hasUnsentDraft: boolean; isFavorite: boolean }> = {},
  ): boolean =>
    sessionIsPeeked(row, now, {
      isUnread: false,
      hasUnsentDraft: false,
      isFavorite: false,
      ...flags,
    });

  it('never hides a session the human starred, however old it is', () => {
    expect(peek(aged(400 * 24 * HOUR))).toBe(false);
    expect(peek(aged(400 * 24 * HOUR), { isFavorite: true })).toBe(true);
  });

  it('never hides an unread answer, however old the session is', () => {
    expect(peek(aged(5 * HOUR), { isUnread: true })).toBe(true);
    expect(peek(aged(400 * 24 * HOUR), { isUnread: true })).toBe(true);
  });

  it('never hides a running session, whatever its clock says', () => {
    expect(peek(aged(5 * HOUR, { live: true }))).toBe(true);
    expect(peek(aged(5 * HOUR, { status: 'running' }))).toBe(true);
    // A gateway that reports liveness explicitly wins over a stale status.
    expect(peek(aged(5 * HOUR, { live: true, status: 'idle' }))).toBe(true);
  });

  it('never hides a session that is waiting for the human', () => {
    expect(peek(aged(5 * HOUR, { status: 'suspended' }))).toBe(true);
  });

  it('keeps unsent words and anything touched within the hour', () => {
    expect(peek(aged(5 * HOUR), { hasUnsentDraft: true })).toBe(true);
    expect(peek(aged(59 * 60 * 1000))).toBe(true);
  });

  it('hides only what is old, idle and already read', () => {
    expect(peek(aged(2 * HOUR))).toBe(false);
    expect(peek(session('s1'))).toBe(false);
    expect(peek(session('s1', { modified_at: 'not a date' }))).toBe(false);
  });

  it('ages off the created/last-active fallbacks too', () => {
    expect(peek(session('s1', { last_active_at: new Date(now - 2 * HOUR).toISOString() }))).toBe(false);
    expect(peek(session('s1', { created_at: new Date(now - 30 * 1000).toISOString() }))).toBe(true);
  });
});

describe('showsScopeStrip', () => {
  it('is the strip, and therefore who states the tallies', () => {
    expect(showsScopeStrip([machine(studio, []), machine(tower, [])])).toBe(true);
    // Solo: no strip, so the header line is the only surface left to count on.
    expect(showsScopeStrip([machine(studio, [])])).toBe(false);
    expect(showsScopeStrip([])).toBe(false);
  });
});

// Unsent work lives on THIS device only, and the session holding it is usually
// empty, so it sorts below everything else there is. So it is pinned to the top
// of the list instead, or you never see it again. A STAR is stronger still: it is
// the one piece of ordering the human typed in themselves, so it outranks live,
// unread, unsent and age, and the starred band must come out the same way
// however many stars there are, however the gateway happened to list them, and
// whether or not the engine's sort is stable.
describe('sessionOrder', () => {
  const order = (
    rows: Session[],
    stars: Record<string, number> = {},
    dirty: Set<string> = new Set<string>(),
  ): string[] =>
    sessionOrder(rows, {
      favoriteRank: (row) => stars[row.id] ?? null,
      hasDraftMessage: (row) => dirty.has(row.id),
    }).map((row) => row.id);

  it('floats the rows holding unsent work, keeping the gateway order inside each half', () => {
    const rows = [session('a'), session('b'), session('c'), session('d')];
    expect(order(rows, {}, new Set(['b', 'd']))).toEqual(['b', 'd', 'a', 'c']);
  });

  it('leaves a list with nothing starred and nothing unsent exactly as it came', () => {
    const rows = [session('a'), session('b')];
    expect(
      sessionOrder(rows, { favoriteRank: () => null, hasDraftMessage: () => false }),
    ).toBe(rows);
  });

  it('pins the stars above unsent work, in the order they were starred', () => {
    const rows = [session('a'), session('b'), session('c'), session('d')];
    expect(order(rows, { d: 1, b: 2 }, new Set(['c']))).toEqual(['d', 'b', 'c', 'a']);
  });

  it('orders the starred band identically however the gateway listed it', () => {
    const rows = ['a', 'b', 'c', 'd', 'e', 'f'].map((id) => session(id));
    const stars = { e: 3, a: 1, c: 2 };
    expect(order(rows, stars)).toEqual(['a', 'c', 'e', 'b', 'd', 'f']);
    expect(order([...rows].reverse(), stars).slice(0, 3)).toEqual(['a', 'c', 'e']);
  });

  it('never reshuffles the stars already there when one more is added', () => {
    const rows = Array.from({ length: 12 }, (_, i) => session(`s${i}`));
    const stars: Record<string, number> = {};
    const bands: string[][] = [];
    for (const [rank, id] of ['s7', 's2', 's9', 's0', 's5'].entries()) {
      stars[id] = rank + 1;
      bands.push(order(rows, stars).slice(0, rank + 1));
    }
    expect(bands).toEqual([
      ['s7'],
      ['s7', 's2'],
      ['s7', 's2', 's9'],
      ['s7', 's2', 's9', 's0'],
      ['s7', 's2', 's9', 's0', 's5'],
    ]);
  });

  it('breaks a shared rank by id, so two runs can never disagree', () => {
    const rows = [session('b'), session('a')];
    expect(order(rows, { a: 4, b: 4 })).toEqual(['a', 'b']);
  });
});

// Paging shortens a long list, and "show more" must never be the thing that hides
// a favorite: stars sort to the front of their own machine, but a project group
// concatenates machines, so a star CAN land past the page boundary.
describe('pageSessions', () => {
  const rows = ['a', 'b', 'c', 'd', 'e'].map((id) => session(id));

  it('returns the list untouched when it fits', () => {
    expect(pageSessions(rows, 5, () => false)).toBe(rows);
  });

  it('cuts to the page when nothing starred is below it', () => {
    expect(pageSessions(rows, 2, () => false).map((row) => row.id)).toEqual(['a', 'b']);
  });

  it('keeps the stars the cut would have eaten, after the page', () => {
    expect(pageSessions(rows, 2, (row) => row.id === 'e').map((row) => row.id)).toEqual([
      'a',
      'b',
      'e',
    ]);
  });
});

// Regression: the start menu's "Or a draft you parked" list never arrived on a
// phone — it sat on "Reading drafts..." forever, and a menu opened before that
// machine's session list had landed latched "No drafts parked in this project
// yet.". The read was keyed on the OBJECT identity of the target machine (a
// background poll replaces it) and of the anchored menu position (the iOS
// keyboard fires `resize` in the very tap that opens the menu), so the in-flight
// request was aborted and restarted on every one of those frames.
describe('draftsRead', () => {
  const parked = session('s1', { workspace: { root: '/Users/me/vis' } });

  it('reads the parked list through the probe session on the target machine', () => {
    expect(draftsRead(machine(studio, [parked]), parked)).toEqual({
      kind: 'read',
      conn: studio,
      sid: 's1',
      repo: '/Users/me/vis',
    });
  });

  it('has nothing to read through a session that names no workspace', () => {
    expect(draftsRead(machine(studio, [session('s1')]), session('s1'))).toEqual({ kind: 'none' });
  });

  it('keys a read by machine and probe, so a poll replacing the objects never restarts it', () => {
    const before = machine(studio, [parked]);
    const after = machine({ ...studio }, [{ ...parked }]);
    // Exactly what a background refresh hands the screen: same read, new objects.
    expect(after).not.toBe(before);
    expect(after.sessions?.[0]).not.toBe(before.sessions?.[0]);
    expect(draftsReadKey(draftsRead(after, after.sessions![0]))).toBe(
      draftsReadKey(draftsRead(before, before.sessions![0])),
    );
  });

  it('re-keys when the new session would be created on another machine', () => {
    expect(draftsReadKey(draftsRead(machine(tower, [parked]), parked))).not.toBe(
      draftsReadKey(draftsRead(machine(studio, [parked]), parked)),
    );
  });

  it('waits while that machine is still loading instead of reporting no drafts', () => {
    expect(draftsRead(machine(studio, null), null)).toEqual({ kind: 'wait' });
    expect(draftsReadKey(draftsRead(machine(studio, null), null))).toBe('wait');
    expect(draftsRead(machine(studio, []), null)).toEqual({ kind: 'none' });
    expect(draftsRead(null, null)).toEqual({ kind: 'none' });
  });

  // Regression (reported: "I click new session, I get the machine, and then I'm seeing
  // those drafts — it's always jumping because it drops from the gateway. Do we have to
  // do the same request for every new session?"): the read was keyed on the PROBE
  // SESSION, and "New session" mints a row in that very repo which then becomes the
  // probe. Same machine, same repo, same list — but a brand-new key, so nothing could
  // ever be reused and the picker fell back to "Reading drafts..." every single time.
  it('keeps one key when a newer session in the same repo becomes the probe', () => {
    const repo = { root: '/Users/me/vis' };
    const parkedIn = session('s1', { workspace: repo });
    const minted = session('s2', { workspace: repo });
    expect(draftsReadKey(draftsRead(machine(studio, [minted, parkedIn]), minted))).toBe(
      draftsReadKey(draftsRead(machine(studio, [parkedIn]), parkedIn)),
    );
  });

  it('re-keys when the next session would be read out of another repo', () => {
    const here = session('s1', { workspace: { root: '/Users/me/vis' } });
    const there = session('s2', { workspace: { root: '/Users/me/spel' } });
    expect(draftsReadKey(draftsRead(machine(studio, [here]), here))).not.toBe(
      draftsReadKey(draftsRead(machine(studio, [there]), there)),
    );
  });

  // A draft's clone is not a project: the drafts of `~/.vis/drafts/vis/x` are the
  // drafts of `~/vis`, so a session sitting in one reads the same list under the
  // same key as a session on trunk.
  it('keys a session parked in a draft under the repo the draft belongs to', () => {
    const trunk = session('s1', { workspace: { root: '/Users/me/vis' } });
    const inDraft = session('s2', {
      workspace: { root: '/Users/me/.vis/drafts/vis/wire', repo_root: '/Users/me/vis', is_draft: true },
    });
    expect(draftsReadKey(draftsRead(machine(studio, [inDraft]), inDraft))).toBe(
      draftsReadKey(draftsRead(machine(studio, [trunk]), trunk)),
    );
  });
});

describe('projectDelete', () => {
  it('deletes a real project recursively when every row agrees on the id', () => {
    const rows = [
      session('a', { project_id: 'p1' }),
      session('b', { project_id: 'p1' }),
    ];
    expect(projectDelete(rows)).toEqual({
      kind: 'project',
      projectId: 'p1',
      sessionIds: ['a', 'b'],
    });
  });

  it('never claims a project for a label-only or mixed group', () => {
    expect(projectDelete([session('a'), session('b')])).toEqual({
      kind: 'sessions',
      sessionIds: ['a', 'b'],
    });
    expect(
      projectDelete([session('a', { project_id: 'p1' }), session('b')]),
    ).toEqual({ kind: 'sessions', sessionIds: ['a', 'b'] });
    expect(
      projectDelete([
        session('a', { project_id: 'p1' }),
        session('b', { project_id: 'p2' }),
      ]),
    ).toEqual({ kind: 'sessions', sessionIds: ['a', 'b'] });
    expect(projectDelete([])).toEqual({ kind: 'sessions', sessionIds: [] });
  });

  it('covers the hidden rows too, not just the ones the list paints', () => {
    const rows = [
      session('named', { project_id: 'p1' }),
      { id: 'hidden', title: '', project_id: 'p1' } as Session,
    ];
    expect(
      rows.filter((row) => sessionIsListed(row, { hasDraftMessage: false, isFavorite: false })),
    ).toHaveLength(1);
    expect(projectDelete(rows).sessionIds).toEqual(['named', 'hidden']);
  });
});

// Regression (reported: "I click new session, I click the machine, I pick my new
// draft, then I go outside of that — and clicking new session once again creates a
// session automatically, which is wrong. I should go over the same dialogs again"):
// the picked machine lived in a state of its own that the name dialog's dismissal
// never cleared, so the next tap found a leftover answer and created the session
// without asking anything.
describe('StartFlow', () => {
  const at = { top: 120, left: 40 };

  it('opens with nothing answered yet', () => {
    const open = startFlowOpen(START_IDLE, at);
    expect(open).toEqual({ step: 'menu', at, on: null });
    expect(startFlowOn(open)).toBeNull();
  });

  it('remembers which machine the order is for, up to the name dialog', () => {
    const picked = startFlowPick(startFlowOpen(START_IDLE, at), tower);
    expect(startFlowOn(picked)).toBe(tower);
    expect(startFlowOn(startFlowName(tower, true))).toBe(tower);
  });

  it('forgets the picked machine when the order is left', () => {
    const named = startFlowName(tower, false);
    expect(startFlowOn(START_IDLE)).toBeNull();
    // Dismissing the name dialog IS leaving the order: the next tap must ask again.
    expect(startFlowOn(startFlowOpen(START_IDLE, at))).toBeNull();
    expect(named).not.toEqual(START_IDLE);
  });

  it('survives a re-anchor, because a resize is not an answer', () => {
    const picked = startFlowPick(startFlowOpen(START_IDLE, at), studio);
    const moved = startFlowOpen(picked, { top: 200, left: 40 });
    expect(moved).toEqual({ step: 'menu', at: { top: 200, left: 40 }, on: studio });
  });

  it('ends the order when there is no anchor left to hang the menu from', () => {
    expect(startFlowOpen(startFlowPick(startFlowOpen(START_IDLE, at), studio), null)).toBe(
      START_IDLE,
    );
  });

  it('ignores a machine picked while no menu is asking', () => {
    expect(startFlowPick(START_IDLE, tower)).toBe(START_IDLE);
    const named = startFlowName(studio, true);
    expect(startFlowPick(named, tower)).toBe(named);
  });
});
