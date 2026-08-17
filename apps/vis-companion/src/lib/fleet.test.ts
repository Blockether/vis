import { describe, expect, it } from 'vitest';
import {
  groupByWorkDir,
  creatableMachines,
  draftsRead,
  draftsReadKey,
  fleetError,
  isFleetLoaded,
  isDraftWorkspace,
  machineCounts,
  machineKey,
  machineLabel,
  newSessionTarget,
  projectDelete,
  projectLabel,
  projectPage,
  reconcileMachines,
  resolveScope,
  SCOPE_ALL,
  scopedMachines,
  scopedConns,
  searchFanout,
  SEARCH_UNPLACED,
  searchOrder,
  searchTally,
  scopedSessions,
  sessionIsEmpty,
  sessionIsListed,
  sessionOrder,
  timeLabel,
  withSearchHits,
  startAsk,
  START_IDLE,
  startFlowName,
  startFlowOn,
  startFlowOpen,
  startFlowPick,
  startFlowUnpick,
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
  return { conn, sessions, error, answered: error === null && sessions !== null };
}

describe('machineLabel', () => {
  it('prefers the pairing label and falls back to the host', () => {
    expect(machineLabel(studio)).toBe('studio');
    expect(machineLabel(tower)).toBe('tower.local:7890');
    expect(machineLabel({ url: 'http://tower.local:7890', label: '   ' })).toBe('tower.local:7890');
  });
});

describe('isDraftWorkspace', () => {
  it('is false when the session names no workspace', () => {
    expect(isDraftWorkspace(session('x'))).toBe(false);
  });
  it('trusts the gateway is_draft flag when present', () => {
    expect(
      isDraftWorkspace(session('x', { workspace: { root: '/vis', is_draft: true } })),
    ).toBe(true);
    expect(
      isDraftWorkspace(session('x', { workspace: { root: '/vis', is_draft: false } })),
    ).toBe(false);
  });
  it('falls back to the drafts path for a gateway without the flag', () => {
    expect(
      isDraftWorkspace(session('x', { workspace: { root: '/Users/me/.vis/drafts/vis/wire' } })),
    ).toBe(true);
    expect(
      isDraftWorkspace(session('x', { workspace: { root: '/Users/me/vis' } })),
    ).toBe(false);
  });
});

describe('groupByWorkDir', () => {
  // Regression, issue #session-list-work-dir: project names used to split one working directory into separate groups.
  it('groups sessions by workspace root even when their project names differ', () => {
    const rows = [
      session('a', {
        project_name: 'first-name',
        modified_at: '2024-05-01T10:00:00Z',
        workspace: { root: '/Users/me/vis' },
      }),
      session('b', {
        project_name: 'second-name',
        modified_at: '2024-04-01T10:00:00Z',
        workspace: { root: '/Users/me/vis' },
      }),
      session('c', {
        project_name: 'other',
        modified_at: '2024-03-01T10:00:00Z',
        workspace: { root: '/Users/me/other' },
      }),
    ];

    expect(groupByWorkDir(rows)).toEqual([
      ['/Users/me/vis', [rows[0], rows[1]]],
      ['/Users/me/other', [rows[2]]],
    ]);
  });

  // Regression, user report ("the way we sort the projects is non-deterministic"): the groups came
  // back in the order the Map happened to be filled, so a project sat wherever its best-ranked
  // SESSION sat. One row starred on this device, one unsent draft, or one turn starting anywhere in
  // the fleet teleported the whole project header — and two devices paired to the same machine
  // painted the projects in two different orders from identical data.
  const roots = (rows: Session[]) => groupByWorkDir(rows).map(([root]) => root);

  it('orders projects by their own recency, whatever order the rows arrive in', () => {
    const old = session('old', {
      modified_at: '2024-01-01T00:00:00Z',
      workspace: { root: '/Users/me/old' },
    });
    const fresh = session('fresh', {
      modified_at: '2024-06-01T00:00:00Z',
      workspace: { root: '/Users/me/fresh' },
    });

    expect(roots([old, fresh])).toEqual(['/Users/me/fresh', '/Users/me/old']);
    expect(roots([fresh, old])).toEqual(['/Users/me/fresh', '/Users/me/old']);
  });

  it('reads a project’s recency from its newest session, not its first row', () => {
    const stale = session('stale', {
      modified_at: '2024-01-01T00:00:00Z',
      workspace: { root: '/Users/me/busy' },
    });
    const newest = session('newest', {
      modified_at: '2024-09-01T00:00:00Z',
      workspace: { root: '/Users/me/busy' },
    });
    const other = session('other', {
      modified_at: '2024-06-01T00:00:00Z',
      workspace: { root: '/Users/me/quiet' },
    });

    expect(roots([stale, other, newest])).toEqual(['/Users/me/busy', '/Users/me/quiet']);
  });

  it('puts a project with work running above a more recent idle one', () => {
    const running = session('running', {
      live: true,
      modified_at: '2024-01-01T00:00:00Z',
      workspace: { root: '/Users/me/running' },
    });
    const idle = session('idle', {
      modified_at: '2024-06-01T00:00:00Z',
      workspace: { root: '/Users/me/idle' },
    });

    expect(roots([idle, running])).toEqual(['/Users/me/running', '/Users/me/idle']);
  });

  it('breaks a tie on the workspace root, so the order is total', () => {
    const at = '2024-06-01T00:00:00Z';
    const b = session('b', { modified_at: at, workspace: { root: '/Users/me/b' } });
    const a = session('a', { modified_at: at, workspace: { root: '/Users/me/a' } });

    expect(roots([b, a])).toEqual(['/Users/me/a', '/Users/me/b']);
    expect(roots([a, b])).toEqual(['/Users/me/a', '/Users/me/b']);
  });
});

describe('projectLabel', () => {
  // Regression, user report ("the way we sort the projects is non-deterministic"): the header read
  // its name off `sessions[0]`, so a group whose rows disagree renamed itself whenever the row order
  // moved. A name the whole group does not agree on is not the project's name — the folder is.
  it('uses a name only when every session in the group agrees on it', () => {
    const rows = [
      session('a', { project_name: 'first-name', workspace: { root: '/Users/me/vis' } }),
      session('b', { project_name: 'second-name', workspace: { root: '/Users/me/vis' } }),
    ];

    expect(projectLabel(rows)).toBe('vis');
    expect(projectLabel([rows[1]!, rows[0]!])).toBe('vis');
    expect(projectLabel([rows[0]!])).toBe('first-name');
  });

  it('names an unnamed group after its folder, and a rootless one at all', () => {
    expect(projectLabel([session('a', { workspace: { root: '/Users/me/vis' } })])).toBe('vis');
    expect(projectLabel([session('a')])).toBe('No project');
    expect(projectLabel([])).toBe('No project');
  });

  // A draft's `workspace.label` is the DRAFT's name; using it as the project name gave every draft
  // its own bogus top-level project.
  it('never takes its name from a draft workspace label', () => {
    const draft = session('a', {
      workspace: {
        root: '/Users/me/.vis/drafts/vis/wire',
        repo_root: '/Users/me/vis',
        label: 'wire',
      },
    });

    expect(projectLabel([draft])).toBe('vis');
  });
});

describe('reconcileMachines', () => {
  it('keeps loaded rows across a re-pair, drops removed machines, blanks new ones', () => {
    const loaded = machine(studio, [session('a')]);
    const next = reconcileMachines([studio, vps], [loaded, machine(tower, [session('b')])]);
    expect(next).toHaveLength(2);
    expect(next[0]).toBe(loaded);
    expect(next[1]).toEqual({ conn: vps, sessions: null, error: null, answered: false });
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

  // Regression, user report ("offline stuff should just not be accessible"): `All` was
  // every PAIRED machine, so a gateway that was not answering took a named section in
  // the middle of the fleet whose entire content was its own failure.
  it('leaves a machine that is not answering out of the fleet view', () => {
    const half = [machines[0], machine(tower, null, 'offline')];
    expect(scopedMachines(half, null)).toEqual([half[0]]);
    expect(scopedSessions(half, null).map((s) => s.id)).toEqual(['a', 'b']);
    // Named, it is still itself: the scope the reader typed is never second-guessed.
    expect(scopedMachines(half, machineKey(tower))).toEqual([half[1]]);
  });

  // Regression, user report ("gateways that are not active should not show up in All —
  // it should only appear once the gateway answers, not appear and then be detached"):
  // a machine painted from its CACHED list took a section on the first frame and lost
  // it seconds later when its probe timed out, so every open of the list flashed a
  // machine that had been asleep for days.
  it('keeps a machine out of the fleet view until it has answered', () => {
    const cachedOnly: FleetMachine = { conn: tower, sessions: [session('c')], error: null, answered: false };
    const half = [machines[0], cachedOnly];
    expect(scopedMachines(half, null)).toEqual([machines[0]]);
    expect(scopedSessions(half, null).map((s) => s.id)).toEqual(['a', 'b']);
    // Named, it is still itself — the reader's own scope is never second-guessed.
    expect(scopedMachines(half, machineKey(tower))).toEqual([cachedOnly]);
    // And it walks in the moment it speaks.
    expect(scopedMachines([machines[0], { ...cachedOnly, answered: true }], null)).toHaveLength(2);
  });

  it('is empty while the fleet is still being tried, so nothing flashes in', () => {
    const cold = [
      { ...machines[0], answered: false },
      { conn: tower, sessions: null, error: null, answered: false } as FleetMachine,
    ];
    expect(scopedMachines(cold, null)).toEqual([]);
  });

  it('keeps every machine when nothing answers, so the blackout has somewhere to be said', () => {
    const dark = [machine(studio, null, 'refused'), machine(tower, null, 'offline')];
    expect(scopedMachines(dark, null)).toEqual(dark);
    expect(fleetError(dark)).toBe('refused');
  });

  it('is loaded only once every machine in scope has answered', () => {
    const half = [machine(studio, [session('a')]), machine(tower, null)];
    expect(isFleetLoaded(half, null)).toBe(false);
    expect(isFleetLoaded(half, studio.url)).toBe(true);
    expect(isFleetLoaded([machine(tower, null, 'offline')], null)).toBe(true);
    expect(isFleetLoaded([], null)).toBe(false);
  });
});

describe('resolveScope', () => {
  const fleet = [machine(studio, [session('a')]), machine(tower, [session('b')])];

  it('answers with the machine the reader named', () => {
    expect(resolveScope(fleet, machineKey(tower))).toBe(machineKey(tower));
    expect(resolveScope(fleet, SCOPE_ALL)).toBe(SCOPE_ALL);
    expect(resolveScope(fleet, 'http://gone.local:7890')).toBe(SCOPE_ALL);
  });

  // Regression, user report ("offline stuff should just not be accessible"): a machine
  // that died under the reading thumb kept the scope, so the list it was showing was
  // replaced by that machine's failure page.
  it('falls back to All when the machine being read stops answering', () => {
    const died = [fleet[0], machine(tower, [session('b')], 'offline')];
    expect(resolveScope(died, machineKey(tower))).toBe(SCOPE_ALL);
  });

  it('a fleet of one resolves to its machine, answering or not', () => {
    expect(resolveScope([machine(studio, null, 'offline')], SCOPE_ALL)).toBe(machineKey(studio));
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

describe('search across the fleet', () => {
  it('an unscoped search targets every paired gateway, a scoped one targets that gateway', () => {
    const conns = [studio, tower, vps];
    expect(scopedConns(conns, null)).toEqual(conns);
    expect(scopedConns(conns, tower.url)).toEqual([tower]);
    // A scope left over from an unpaired machine must not silence the search.
    expect(scopedConns(conns, 'http://gone.local:7890')).toEqual(conns);
  });

  // Regression, user report (paraphrased: "make sure we are not putting search requests to
  // machines that are genuinely dead"): every paired gateway was asked, so a machine that
  // had failed its list read — or had already let a whole search deadline pass in silence —
  // was asked again, and the reader waited on it again.
  it('puts the question only to machines that can still answer one', () => {
    const conns = [studio, tower, vps];
    const fleet = [machine(studio, [session('a')]), machine(tower, null, 'offline'), machine(vps, [session('b')])];
    const fanout = searchFanout(conns, fleet, SCOPE_ALL, new Set([machineKey(vps)]));
    expect(fanout.ask).toEqual([studio]);
    // Both kinds of dark are still ASKED: a fleet that shrinks to the machines that work
    // reports a search as complete that never read half of it.
    expect(fanout.asked).toEqual([machineKey(studio), machineKey(tower), machineKey(vps)]);
    expect(fanout.dark).toEqual([machineKey(tower), machineKey(vps)]);
  });

  it('narrows to the scope, and asks a live machine even while another is dark', () => {
    const conns = [studio, tower];
    const fleet = [machine(studio, [session('a')]), machine(tower, null, 'offline')];
    expect(searchFanout(conns, fleet, machineKey(studio), new Set()).ask).toEqual([studio]);
    // Scoped to the dark machine, nothing is asked and the machine is still counted.
    const onDark = searchFanout(conns, fleet, machineKey(tower), new Set());
    expect(onDark.ask).toEqual([]);
    expect(onDark.asked).toEqual([machineKey(tower)]);
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

  // Regression, user report (paraphrased: "the search results are not sorted by
  // freshness — I care about freshness, not the band a hit landed in"). The app
  // used to sort matched rows by `SessionMatch.rank`, so every year-old title
  // holding the word sat above the session touched this morning. It now paints
  // the PLACE the gateway gave each match: the gateway's own freshest-first
  // order.

  it('paints the gateway order — the place of a match, never its relevance band', () => {
    const rows = [session('reply'), session('ask'), session('named'), session('other')];
    // The gateway answered freshest first; the BANDS run the other way.
    const place: Record<string, number> = { reply: 0, ask: 1, other: 2, named: 3 };
    expect(searchOrder(rows, (row) => place[row.id] ?? 9).map((row) => row.id)).toEqual([
      'reply',
      'ask',
      'other',
      'named',
    ]);
  });

  it('keeps the incoming order between rows the gateway placed together', () => {
    const rows = [session('second'), session('first')];
    expect(searchOrder(rows, () => 0).map((row) => row.id)).toEqual(['second', 'first']);
  });

  it('sorts a row the gateway did not place after every placed one', () => {
    const rows = [session('draft-only'), session('thinking')];
    const place: Record<string, number> = { thinking: 7 };
    expect(
      searchOrder(rows, (row) => place[row.id] ?? SEARCH_UNPLACED).map((row) => row.id),
    ).toEqual(['thinking', 'draft-only']);
    expect(SEARCH_UNPLACED).toBeGreaterThan(1_000_000);
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

// Regression (reported in-app: "I have the problem with searching the sessions …
// on iOS"): the session list is paged, and the filter intersected the gateway's
// server-side transcript hits with the rows already loaded — a match in a session
// that had not been paged in was silently dropped, so search only ever found what
// was already on screen.
describe('withSearchHits', () => {
  it('adds the hit sessions the paged list has not loaded, newest first', () => {
    const loaded = [session('a', { modified_at: '2024-05-02T10:00:00Z' })];
    const hits = [
      session('a', { modified_at: '2024-05-02T10:00:00Z' }),
      session('old', { modified_at: '2024-01-01T10:00:00Z' }),
      session('newer', { modified_at: '2024-04-01T10:00:00Z' }),
    ];
    expect(withSearchHits(loaded, hits).map((row) => row.id)).toEqual(['a', 'newer', 'old']);
  });

  it('keeps the list identical when there is nothing to hydrate', () => {
    const loaded = [session('a')];
    expect(withSearchHits(loaded, [])).toBe(loaded);
    expect(withSearchHits(loaded, [session('a')])).toBe(loaded);
  });

  // Regression, user report (paraphrased: "opening a session suddenly makes it the
  // freshest one and it jumps up"): ranking read `last_active_at`, the gateway's
  // touch clock, so a session merely read outranked one that had actually changed.
  it('ranks hydrated hits by content time, never by the last touch', () => {
    const loaded = [session('a', { modified_at: '2024-05-02T10:00:00Z' })];
    const hits = [
      session('touched', {
        created_at: '2024-01-01T10:00:00Z',
        last_active_at: '2024-05-02T11:00:00Z',
      }),
      session('changed', { modified_at: '2024-03-01T10:00:00Z' }),
    ];
    expect(withSearchHits(loaded, hits).map((row) => row.id)).toEqual([
      'a',
      'changed',
      'touched',
    ]);
  });
});

describe('timeLabel', () => {
  const now = Date.parse('2024-05-02T12:00:00Z');

  it('stays relative inside a day', () => {
    expect(timeLabel('2024-05-02T09:00:00Z', now)).toMatch(/hour/);
    expect(timeLabel('2024-05-02T11:40:00Z', now)).toMatch(/minute/);
  });

  it('names the actual date once the row is older than a day', () => {
    const label = timeLabel('2024-04-20T08:30:00Z', now);
    expect(label).toMatch(/20/);
    expect(label).toMatch(/:/);
    expect(label).not.toMatch(/ago/);
  });

  it('adds the year only when it is not this one', () => {
    expect(timeLabel('2023-11-04T08:30:00Z', now)).toMatch(/2023/);
    expect(timeLabel('2024-04-20T08:30:00Z', now)).not.toMatch(/2024/);
  });

  it('has nothing to say about a missing stamp', () => {
    expect(timeLabel(undefined, now)).toBe('-');
    expect(timeLabel('not a date', now)).toBe('-');
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

// Regression, user report ("when I am going to the latest page on the session list
// there is a very unpleasant reflow and flicker"): the page count came from these
// filtered rows while pages 2 and up were re-fetched from the gateway's own unfiltered
// window at the same offset, so the last page painted three rows and then swapped them
// for ten that belonged to a different list.
describe('projectPage', () => {
  const rows = (count: number) => Array.from({ length: count }, (_, at) => session(`s${at}`));

  it('cuts the page out of the rows it was handed', () => {
    const list = rows(23);
    expect(projectPage(list, 1, 10).rows.map((row) => row.id)).toEqual(
      list.slice(0, 10).map((row) => row.id),
    );
    expect(projectPage(list, 2, 10).rows.map((row) => row.id)).toEqual(
      list.slice(10, 20).map((row) => row.id),
    );
  });

  it('ends where the list ends: the last page is as short as it truly is', () => {
    const page = projectPage(rows(763), 77, 10);
    expect(page.pageCount).toBe(77);
    expect(page.rows).toHaveLength(3);
    expect(page.rows.map((row) => row.id)).toEqual(['s760', 's761', 's762']);
  });

  it('clamps a page the list no longer has instead of painting an empty band', () => {
    expect(projectPage(rows(12), 99, 10)).toMatchObject({ page: 2, pageCount: 2 });
    expect(projectPage(rows(12), 0, 10)).toMatchObject({ page: 1, pageCount: 2 });
    expect(projectPage([], 3, 10)).toMatchObject({ page: 1, pageCount: 1, rows: [] });
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

  // The menu's own Back: the machine question again, with the order still open. It only
  // exists because that answer was given HERE — a draft question opened from a project
  // header was never asked which machine, and offers no Back at all.
  it('takes the picked machine back without ending the order', () => {
    const picked = startFlowPick(startFlowOpen(START_IDLE, at), tower);
    const back = startFlowUnpick(picked);
    expect(back).toEqual({ step: 'menu', at, on: null });
    expect(startFlowOn(back)).toBeNull();
    // Nothing else un-picks: a name dialog is past the question, not inside it.
    const named = startFlowName(studio, true);
    expect(startFlowUnpick(named)).toBe(named);
    expect(startFlowUnpick(START_IDLE)).toBe(START_IDLE);
  });

  it('ignores a machine picked while no menu is asking', () => {
    expect(startFlowPick(START_IDLE, tower)).toBe(START_IDLE);
    const named = startFlowName(studio, true);
    expect(startFlowPick(named, tower)).toBe(named);
  });
});
