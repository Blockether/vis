import { describe, expect, it } from 'vitest';
import {
  groupByWorkDir,
  fleetError,
  fleetRead,
  isDraftWorkspace,
  machineCounts,
  machineKey,
  machineLabel,
  machineRead,
  projectGroups,
  projectLabel,
  readSinceCounted,
  machineTally,
  reconcileMachines,
  resolveScope,
  SCOPE_ALL,
  scopedMachines,
  scopedConns,
  searchFanout,
  searchGroups,
  searchTally,
  scopedSessions,
  sessionInputCount,
  sessionIsLive,
  sessionOrder,
  servedUnread,
  timeLabel,
  type FleetMachine,
} from './fleet';
import type { GatewayConn, GatewayOverview, Session } from './types';
import { unreadTurnCount } from './unread';

const studio: GatewayConn = { url: 'http://studio.local:7890', label: 'studio' };
const tower: GatewayConn = { url: 'http://tower.local:7890' };
const vps: GatewayConn = { url: 'http://10.0.0.5:7890', label: 'vps-eu' };

function session(id: string, extra: Partial<Session> = {}): Session {
  return {
    id,
    title: id,
    live: false,
    current_turn_id: null,
    turn_count: 0,
    server_time_ms: 0,
    ...extra,
  };
}

// Regression, user report (paraphrased: the INPUT NEEDED mark does not go away
// when I answer): a session parked on TWO requests carried one boolean, so
// answering the first of them changed nothing the reader could see.
describe('open input requests', () => {
  it('counts the requests the gateway reports', () => {
    const parked = session('s', { is_awaiting_input: true, awaiting_input_count: 2 });
    expect(sessionInputCount(session('s'))).toBe(0);
    expect(sessionInputCount(parked)).toBe(2);
  });

  it('still counts a parked row from a gateway that sends no count', () => {
    expect(sessionInputCount(session('s', { is_awaiting_input: true }))).toBe(1);
  });
});

describe('canonical session liveness', () => {
  it('never infers a running turn from display status', () => {
    expect(sessionIsLive(session('s', { status: 'running' }))).toBe(false);
    expect(sessionIsLive(session('s', { live: true, status: 'idle' }))).toBe(true);
  });
});
function machine(
  conn: GatewayConn,
  sessions: Session[] | null,
  error: string | null = null,
): FleetMachine {
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
    expect(isDraftWorkspace(session('x', { workspace: { root: '/vis', is_draft: true } }))).toBe(
      true,
    );
    expect(isDraftWorkspace(session('x', { workspace: { root: '/vis', is_draft: false } }))).toBe(
      false,
    );
  });
  it('falls back to the drafts path for a gateway without the flag', () => {
    expect(
      isDraftWorkspace(session('x', { workspace: { root: '/Users/me/.vis/drafts/vis/wire' } })),
    ).toBe(true);
    expect(isDraftWorkspace(session('x', { workspace: { root: '/Users/me/vis' } }))).toBe(false);
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
      ['/Users/me/other', [rows[2]]],
      ['/Users/me/vis', [rows[0], rows[1]]],
    ]);
  });

  // Regression, user report ("the way we sort the projects is non-deterministic"): the groups came
  // back in the order the Map happened to be filled, so a project sat wherever its best-ranked
  // SESSION sat. One row starred on this device, one unsent draft, or one turn starting anywhere in
  // the fleet teleported the whole project header — and two devices paired to the same machine
  // painted the projects in two different orders from identical data.
  const roots = (rows: Session[]) => groupByWorkDir(rows).map(([root]) => root);

  it('orders fallback projects by root regardless of arrival order', () => {
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

  it('keeps fallback order when a session becomes newer', () => {
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

  it('does not lift a running fallback project', () => {
    const running = session('running', {
      live: true,
      modified_at: '2024-01-01T00:00:00Z',
      workspace: { root: '/Users/me/running' },
    });
    const idle = session('idle', {
      modified_at: '2024-06-01T00:00:00Z',
      workspace: { root: '/Users/me/idle' },
    });

    expect(roots([idle, running])).toEqual(['/Users/me/idle', '/Users/me/running']);
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
    const cachedOnly: FleetMachine = {
      conn: tower,
      sessions: [session('c')],
      error: null,
      answered: false,
    };
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

  it('reads as settled only once every machine in scope has answered', () => {
    const half = [machine(studio, [session('a')]), machine(tower, null)];
    expect(fleetRead(half, null)).toBe('reading');
    expect(fleetRead(half, studio.url)).toBe('settled');
    expect(fleetRead([machine(tower, null, 'offline')], null)).toBe('down');
    expect(fleetRead([], null)).toBe('reading');
  });
});

describe('resolveScope', () => {
  const fleet = [machine(studio, [session('a')]), machine(tower, [session('b')])];

  // Regression, user report: the list could start with no machine selected and pressing
  // the selected machine again turned it off.
  it('always resolves to exactly one active machine', () => {
    expect(resolveScope(fleet, machineKey(tower))).toBe(machineKey(tower));
    expect(resolveScope(fleet, SCOPE_ALL)).toBe(machineKey(studio));
    expect(resolveScope(fleet, 'http://gone.local:7890')).toBe(machineKey(studio));
  });

  it('moves to the first answering machine when the active machine stops answering', () => {
    const died = [fleet[0], machine(tower, [session('b')], 'offline')];
    expect(resolveScope(died, machineKey(tower))).toBe(machineKey(studio));
  });

  it('keeps the first machine active when the whole fleet is dark', () => {
    expect(resolveScope([machine(studio, null, 'offline')], SCOPE_ALL)).toBe(machineKey(studio));
  });
});

describe('fleetError', () => {
  it('stays silent while anything still answers', () => {
    expect(
      fleetError([machine(studio, [session('a')]), machine(tower, null, 'offline')]),
    ).toBeNull();
    expect(fleetError([machine(studio, null)])).toBeNull();
    expect(fleetError([])).toBeNull();
  });

  it('reports the first failure when every machine is down', () => {
    expect(fleetError([machine(studio, null, 'refused'), machine(tower, null, 'offline')])).toBe(
      'refused',
    );
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
    expect(
      machineCounts(
        machine(tower, null),
        () => true,
        () => true,
      ),
    ).toEqual({
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
    const fleet = [
      machine(studio, [session('a')]),
      machine(tower, null, 'offline'),
      machine(vps, [session('b')]),
    ];
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

  // Regression, user report: going back from a session to a long list lagged on a
  // phone. Building an `Intl` formatter per row was most of the list's render time.
  it('reuses its formatters instead of building them for every row', () => {
    const stamps = ['2024-05-02T11:40:00Z', '2024-04-20T08:30:00Z', '2023-11-04T08:30:00Z'];
    for (const stamp of stamps) timeLabel(stamp, now);
    let built = 0;
    const counted = <T extends new (...args: never[]) => object>(real: T): T =>
      new Proxy(real, {
        construct(target, args, newTarget) {
          built += 1;
          return Reflect.construct(target, args, newTarget);
        },
      });
    const { RelativeTimeFormat, DateTimeFormat } = Intl;
    Object.assign(Intl, {
      RelativeTimeFormat: counted(RelativeTimeFormat),
      DateTimeFormat: counted(DateTimeFormat),
    });
    try {
      for (let row = 0; row < 50; row += 1) {
        for (const stamp of stamps) timeLabel(stamp, now);
      }
    } finally {
      Object.assign(Intl, { RelativeTimeFormat, DateTimeFormat });
    }
    expect(built).toBe(0);
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

describe('sessionOrder', () => {
  it('puts the newest message first, also above a star', () => {
    const starred = session('starred', { modified_at: '2026-01-01T00:00:00Z', favorite_rank: 1 });
    const sent = session('sent', { modified_at: '2026-01-02T00:00:00Z' });
    expect(sessionOrder([starred, sent]).map((row) => row.id)).toEqual(['sent', 'starred']);
  });

  it('keeps an already ordered list by identity without mutating its rows', () => {
    const rows = [session('a'), session('b')];
    expect(sessionOrder(rows)).toBe(rows);
  });

  it('uses the same id tie-break regardless of incoming order', () => {
    const rows = [session('b'), session('a')];
    expect(sessionOrder(rows).map((row) => row.id)).toEqual(['a', 'b']);
    expect(rows.map((row) => row.id)).toEqual(['b', 'a']);
  });
});

// Paging shortens a long list, and "show more" must never be the thing that hides
// a favorite: stars sort to the front of their own machine, but a project group
// concatenates machines, so a star CAN land past the page boundary.

describe('projectGroups', () => {
  const rows = (extra: Array<Partial<Session>>) =>
    extra.map((fields, index) =>
      session(String.fromCharCode(97 + index), { workspace: { root: '/repo/a' }, ...fields }),
    );

  it('claims the project id when every row of a group agrees on it', () => {
    const groups = projectGroups(null, rows([{ project_id: 'p1' }, { project_id: 'p1' }]));
    expect(groups).toHaveLength(1);
    expect(groups[0]?.projectId).toBe('p1');
    expect(groups[0]?.sessions.map((row) => row.id)).toEqual(['a', 'b']);
  });

  it('never claims a project for a label-only or mixed group', () => {
    expect(projectGroups(null, rows([{}, {}]))[0]?.projectId).toBe('');
    expect(projectGroups(null, rows([{ project_id: 'p1' }, {}]))[0]?.projectId).toBe('');
    expect(
      projectGroups(null, rows([{ project_id: 'p1' }, { project_id: 'p2' }]))[0]?.projectId,
    ).toBe('');
    expect(projectGroups(null, [])).toEqual([]);
  });

  it('orders gateway projects by canonical root without changing their counts or the snapshot', () => {
    const overview: GatewayOverview = {
      projects: [
        {
          root: '/repo/quiet',
          project_id: 'p-q',
          name: 'Quiet',
          session_count: 400,
          live_count: 0,
          awaiting_count: 0,
          last_activity_ms: 900,
        },
        {
          root: '/repo/busy',
          project_id: 'p-b',
          name: 'Busy',
          session_count: 12,
          live_count: 2,
          awaiting_count: 0,
          last_activity_ms: 100,
        },
      ],
      project_count: 2,
      session_count: 412,
      live_count: 2,
      awaiting_count: 0,
    };
    const groups = projectGroups(overview, []);
    expect(groups.map((group) => group.root)).toEqual(['/repo/busy', '/repo/quiet']);
    expect(groups.map((group) => group.tally)).toEqual([
      { count: 12, live: 2, awaiting: 0, unread: 0, stopped: 0 },
      { count: 400, live: 0, awaiting: 0, unread: 0, stopped: 0 },
    ]);
    expect(overview.projects.map((project) => project.root)).toEqual(['/repo/quiet', '/repo/busy']);
    const refreshed = {
      ...overview,
      projects: [...overview.projects].reverse().map((project) => ({
        ...project,
        name: 'Renamed',
        last_activity_ms: 9999,
        live_count: 5,
      })),
    };
    const updated = projectGroups(refreshed, []);
    expect(updated.map((group) => group.root)).toEqual(['/repo/busy', '/repo/quiet']);
    expect(updated.map((group) => [group.label, group.tally.live])).toEqual([
      ['Renamed', 5],
      ['Renamed', 5],
    ]);
    expect(groups.every((group) => group.sessions.length === 0)).toBe(true);
  });

  it('keeps a local root in the same position when its overview arrives', () => {
    const overview: GatewayOverview = {
      projects: [
        {
          root: '/repo/a',
          project_id: 'p-a',
          name: 'A',
          session_count: 9,
          live_count: 0,
          awaiting_count: 0,
          last_activity_ms: 10,
        },
      ],
      project_count: 1,
      session_count: 9,
      live_count: 0,
      awaiting_count: 0,
    };
    const draft = session('d', { workspace: { root: '/drafts/x', is_draft: true } });
    const groups = projectGroups(overview, [draft]);
    expect(groups.map((group) => group.root)).toEqual(['/drafts/x', '/repo/a']);
    expect(groups[0]?.tally).toEqual({ count: 1, live: 0, awaiting: 0, unread: 0, stopped: 0 });
    expect(groups[0]?.sessions).toEqual([draft]);
    overview.projects.unshift({
      root: '/drafts/x',
      project_id: 'p-d',
      name: 'Draft',
      session_count: 1,
      live_count: 0,
      awaiting_count: 0,
      last_activity_ms: 20,
    });
    const refreshed = projectGroups(overview, [draft]);
    expect(refreshed.map((group) => group.root)).toEqual(groups.map((group) => group.root));
    expect(refreshed[0]?.projectId).toBe('p-d');
    expect(refreshed[0]?.sessions).toEqual([draft]);
  });
});

// Regression, user report (paraphrased: switching gateways flickered the project
// list and then the counts inside it): the numbers were a tally of the session
// windows this device had paged in, so they read low and moved as pages landed.
describe('machineTally', () => {
  const overview: GatewayOverview = {
    projects: [
      {
        root: '/repo/a',
        project_id: 'p-a',
        name: 'Vis',
        session_count: 400,
        live_count: 3,
        awaiting_count: 0,
        last_activity_ms: 300,
      },
    ],
    project_count: 1,
    session_count: 412,
    live_count: 4,
    awaiting_count: 1,
  };

  const window = () => [
    session('s1', { live: true, workspace: { root: '/repo/a' } }),
    session('s2', { workspace: { root: '/repo/a' } }),
  ];

  it('says what the GATEWAY holds, not what this device has paged in', () => {
    const groups = projectGroups(overview, window());
    expect(groups[0]?.tally).toEqual({ count: 400, live: 3, awaiting: 0, unread: 0, stopped: 0 });
    expect(machineTally(overview, groups)).toEqual({ count: 412, live: 4 });
  });

  it('falls back to the rows on screen for a machine that has not answered one', () => {
    const groups = projectGroups(null, window());
    expect(groups[0]?.tally).toEqual({ count: 2, live: 1, awaiting: 0, unread: 0, stopped: 0 });
    expect(machineTally(undefined, groups)).toEqual({ count: 2, live: 1 });
  });

  it('falls back for a project the overview does not carry', () => {
    const groups = projectGroups(overview, [session('s9', { workspace: { root: '/repo/gone' } })]);
    expect(groups.find((group) => group.root === '/repo/gone')?.tally).toEqual({
      count: 1,
      live: 0,
      awaiting: 0,
      unread: 0,
      stopped: 0,
    });
  });
});

// Regression, user report (paraphrased: when Council notifications wake sessions, the
// sessions that were new drop out of the project's count): a header counted NEW over the
// head window, and a wake lifts the sessions it runs above every unread conversation.
// A running session never shows NEW on its own row either.
describe('the NEW a header counts', () => {
  const overviewOf = (unread?: number): GatewayOverview => {
    const counted = unread === undefined ? {} : { unread_count: unread };
    return {
      projects: [
        {
          root: '/repo/a',
          project_id: 'p-a',
          name: 'Vis',
          session_count: 40,
          live_count: 2,
          awaiting_count: 0,
          ...counted,
          last_activity_ms: 300,
        },
      ],
      project_count: 1,
      session_count: 40,
      live_count: 2,
      awaiting_count: 0,
      ...counted,
    };
  };
  const inA = { root: '/repo/a' };
  // The window after a wake: the two sessions it runs on top, one of them unread, and one
  // unread conversation left under them. The gateway counts a third the wake pushed out.
  const woken = (): Session[] => [
    session('w1', { live: true, running_request_kind: 'council', workspace: inA }),
    session('w2', {
      live: true,
      running_request_kind: 'council',
      answer_count: 2,
      is_unread: true,
      unread_answers: 1,
      workspace: inA,
    }),
    session('n1', { answer_count: 4, is_unread: true, unread_answers: 1, workspace: inA }),
  ];
  const isUnread = (row: Session) => unreadTurnCount(row) > 0;
  const held = (rows: Session[], overview: GatewayOverview): FleetMachine => ({
    ...machine(studio, rows),
    overview,
    countedUnread: servedUnread(rows),
  });

  it('keeps every conversation the gateway counted while a wake reorders the window', () => {
    const overview = overviewOf(3);
    expect(projectGroups(overview, woken(), isUnread)[0]?.tally.unread).toBe(3);
    expect(machineCounts(held(woken(), overview), sessionIsLive, isUnread).unread).toBe(3);
  });

  it('takes a row read here off the count until the gateway has caught up', () => {
    const rows = woken();
    const before = held(rows, overviewOf(3));
    const seen = readSinceCounted(before, (row) => row.id === 'n1');
    expect(projectGroups(before.overview, rows, isUnread, seen)[0]?.tally.unread).toBe(2);
    expect(machineCounts(before, sessionIsLive, isUnread, seen).unread).toBe(2);

    // The read mark came back: n1 is neither counted nor served as new any more.
    const read = rows.map((row) =>
      row.id === 'n1' ? { ...row, is_unread: false, unread_answers: 0 } : row,
    );
    const after = held(read, overviewOf(2));
    const stillSeen = readSinceCounted(after, (row) => row.id === 'n1');
    expect(projectGroups(after.overview, read, isUnread, stillSeen)[0]?.tally.unread).toBe(2);
    expect(machineCounts(after, sessionIsLive, isUnread, stillSeen).unread).toBe(2);
  });

  it('keeps the served verdict while an answer repeats it', () => {
    const first = servedUnread(woken());
    expect([...first].sort()).toEqual(['n1', 'w2']);
    expect(servedUnread(woken(), first)).toBe(first);
    expect(servedUnread(woken().slice(0, 2), first)).toEqual(new Set(['w2']));
  });

  it('counts the rows on screen for an overview without a NEW count', () => {
    const overview = overviewOf();
    expect(projectGroups(overview, woken(), isUnread)[0]?.tally.unread).toBe(1);
    expect(machineCounts(held(woken(), overview), sessionIsLive, isUnread).unread).toBe(1);
  });
});

// Regression, user report (paraphrased: STOPPED is missing from the group and project
// headers): a header counted a stopped conversation as NEW.
describe('the STOPPED a header counts', () => {
  const inA = { root: '/repo/a' };
  const overviewOf = (stopped?: number): GatewayOverview => {
    const counted = stopped === undefined ? {} : { stopped_count: stopped };
    return {
      projects: [
        {
          root: '/repo/a',
          project_id: 'p-a',
          name: 'Vis',
          session_count: 40,
          live_count: 0,
          awaiting_count: 0,
          unread_count: 3,
          ...counted,
          last_activity_ms: 300,
        },
      ],
      project_count: 1,
      session_count: 40,
      live_count: 0,
      awaiting_count: 0,
      unread_count: 3,
      ...counted,
    };
  };
  // The window holds one stopped and one answered conversation. The gateway also counts
  // a stopped conversation that the window does not hold.
  const rows = (): Session[] => [
    session('s1', {
      answer_count: 2,
      is_unread: true,
      unread_answers: 1,
      was_interrupted: true,
      workspace: inA,
    }),
    session('n1', { answer_count: 4, is_unread: true, unread_answers: 1, workspace: inA }),
  ];
  const isUnread = (row: Session) => unreadTurnCount(row) > 0;

  it('takes the gateway count, also for a conversation outside the window', () => {
    const tally = projectGroups(overviewOf(2), rows(), isUnread)[0]?.tally;
    expect(tally).toMatchObject({ unread: 3, stopped: 2 });
  });

  it('takes a stopped row read here off STOPPED as well as off the unread total', () => {
    const held: FleetMachine = {
      ...machine(studio, rows()),
      overview: overviewOf(2),
      countedUnread: servedUnread(rows()),
    };
    const seen = readSinceCounted(held, (row) => row.id === 's1');
    const tally = projectGroups(held.overview, rows(), isUnread, seen)[0]?.tally;
    expect(tally).toMatchObject({ unread: 2, stopped: 1 });
  });

  it('counts the stopped rows on screen for an overview without a STOPPED count', () => {
    expect(projectGroups(overviewOf(), rows(), isUnread)[0]?.tally.stopped).toBe(1);
  });

  it('counts the stopped rows of a project that no overview counts', () => {
    const failed = session('f1', {
      is_unread: true,
      unread_answers: 1,
      was_failed: true,
      workspace: { root: '/repo/b' },
    });
    const local = projectGroups(overviewOf(0), [failed], isUnread).find((group) => group.root === '/repo/b');
    expect(local?.tally).toMatchObject({ unread: 1, stopped: 1 });
  });
});

// Regression, duplicate project header: a group is keyed by a path, but a row
// NAMES its project with `project_id`. A row whose path disagreed with the
// project it claims minted a second header for a project the overview was
// already counting it in — the same project, twice, one of them nameless.
describe('a row that names its project', () => {
  const overview: GatewayOverview = {
    projects: [
      {
        root: '/Users/me/vis',
        project_id: 'p-vis',
        name: 'vis',
        session_count: 3,
        live_count: 0,
        awaiting_count: 0,
        last_activity_ms: 10,
      },
    ],
    project_count: 1,
    session_count: 3,
    live_count: 0,
    awaiting_count: 0,
  };

  it('paints under that project whatever its own path says', () => {
    const stray = session('s1', {
      project_id: 'p-vis',
      project_name: 'vis',
      workspace: { root: '/Users/me/vis/apps/vis-companion' },
    });
    const groups = projectGroups(overview, [stray]);

    expect(groups.map((group) => group.root)).toEqual(['/Users/me/vis']);
    expect(groups[0]?.sessions).toEqual([stray]);
  });

  it('still falls back to its path when it claims nothing the overview counts', () => {
    const elsewhere = session('s2', {
      project_id: 'p-gone',
      workspace: { root: '/Users/me/other' },
    });
    const groups = projectGroups(overview, [elsewhere]);

    expect(groups.map((group) => group.root)).toEqual(['/Users/me/other', '/Users/me/vis']);
    expect(groups[0]?.sessions).toEqual([elsewhere]);
  });

  // A row with no project id at all keys by the GATEWAY's rule, so a leftover
  // group may still appear — but it must not wear the counted project's name.
  it('never gives a leftover group the counted project name', () => {
    const groups = projectGroups(overview, [
      session('s3', { project_name: 'vis', workspace: { root: '/Users/me/vis-sandbox' } }),
    ]);

    expect(groups.map((group) => [group.root, group.label])).toEqual([
      ['/Users/me/vis', 'vis'],
      ['/Users/me/vis-sandbox', 'vis-sandbox'],
    ]);
  });

  // `searchGroups` has no overview to disagree with: the rows' own name is the
  // only name that group will ever have.
  it('leaves a search group named by its rows', () => {
    const groups = searchGroups([
      session('s4', { project_name: 'vis', workspace: { root: '/Users/me/vis-sandbox' } }),
    ]);

    expect(groups.map((group) => group.label)).toEqual(['vis']);
  });

  // The header names the project and never its path: an older gateway stored the
  // folder path as the project name, and its header must still say only the folder.
  it('names a project whose stored name is a path by its folder', () => {
    const legacy: GatewayOverview = {
      ...overview,
      projects: overview.projects.map((project) => ({ ...project, name: project.root })),
    };
    const groups = projectGroups(legacy, [
      session('s5', { project_name: '~/vis-sandbox', workspace: { root: '/Users/me/vis-sandbox' } }),
    ]);

    expect(groups.map((group) => group.label)).toEqual(['vis', 'vis-sandbox']);
    expect(searchGroups([session('s6', { project_name: 'Wallet app' })])[0]?.label).toBe('Wallet app');
    expect(
      searchGroups([session('s7', { project_name: 'C:\\Users\\me\\billing\\' })])[0]?.label,
    ).toBe('billing');
  });
});

// Regression, duplicate project header: the gateway files EVERY session under its
// workspace `repo_root` when it has one (`state/session-project-root`). Branching
// on `is_draft` here filed an ordinary session opened in a subdirectory of its
// repository under that subdirectory, so the project got a second header holding
// rows the first one was still counting.
describe('the path a session is grouped under', () => {
  it('is the repository root for an ordinary session in a subdirectory', () => {
    const inSubdir = session('s1', {
      workspace: { root: '/Users/me/vis/apps/vis-companion', repo_root: '/Users/me/vis' },
    });
    const atRoot = session('s2', {
      workspace: { root: '/Users/me/vis', repo_root: '/Users/me/vis' },
    });

    expect(groupByWorkDir([inSubdir, atRoot])).toEqual([
      ['/Users/me/vis', [inSubdir, atRoot]],
    ]);
  });

  it('is still the repository root for a draft clone', () => {
    const draft = session('s3', {
      workspace: {
        root: '/Users/me/.vis/drafts/vis/wire',
        repo_root: '/Users/me/vis',
        is_draft: true,
      },
    });

    expect(groupByWorkDir([draft])).toEqual([['/Users/me/vis', [draft]]]);
  });

  it('is the working directory when no repository was resolved', () => {
    const loose = session('s4', { workspace: { root: '/Users/me/scratch/' } });

    expect(groupByWorkDir([loose])).toEqual([['/Users/me/scratch', [loose]]]);
  });
});

// Regression: the screen answered "is this list still loading" three times over, from
// `sessions`, from `answered` and from `error`, and the answers disagreed. Cached rows
// read as an answer, a remembered outage read as an answer in one place and as a
// reconnect in another, and a machine confirmed dark in this run read as still being
// read. One value answers it now, per machine and per scope.
describe('machineRead', () => {
  const cached: FleetMachine = {
    conn: studio,
    sessions: [session('a')],
    error: null,
    answered: false,
  };

  it('waits for the gateway to speak, not for cached rows to exist', () => {
    expect(machineRead(cached)).toBe('reading');
    expect(machineRead({ ...cached, answered: true })).toBe('settled');
  });

  it('separates a failure measured in this run from one this device remembered', () => {
    expect(machineRead({ ...cached, error: 'offline' })).toBe('down');
    expect(machineRead({ ...cached, error: 'offline', isRemembered: true })).toBe('reading');
  });

  it('is reading for a machine nobody has asked yet', () => {
    expect(machineRead({ conn: tower, sessions: null, error: null, answered: false })).toBe(
      'reading',
    );
  });
});

describe('fleetRead', () => {
  const settled = machine(studio, [session('a')]);
  const warm: FleetMachine = { conn: tower, sessions: [session('b')], error: null, answered: false };

  it('keeps a whole fleet reading while one of its machines still is', () => {
    expect(fleetRead([settled, warm], null)).toBe('reading');
    expect(fleetRead([settled, { ...warm, answered: true }], null)).toBe('settled');
  });

  // `All` holds only the machines that have spoken, so asking the drained list would
  // settle a fleet of six the moment the first of them answered.
  it('weighs every paired machine for All, not the machines All shows', () => {
    expect(scopedMachines([settled, warm], null)).toEqual([settled]);
    expect(fleetRead([settled, warm], null)).toBe('reading');
  });

  it('narrows to the one machine a scope names', () => {
    expect(fleetRead([settled, warm], studio.url)).toBe('settled');
    expect(fleetRead([settled, warm], tower.url)).toBe('reading');
  });

  it('reads a fleet with nothing left to try as down, not as settled', () => {
    const dark = [machine(studio, null, 'refused'), machine(tower, null, 'offline')];
    expect(fleetRead(dark, null)).toBe('down');
    // One machine of it still has a reconnect in flight, so the fleet is still reading.
    expect(fleetRead([{ ...dark[0], isRemembered: true }, dark[1]], null)).toBe('reading');
  });

  it('settles when one machine answered and the rest have nothing left to try', () => {
    expect(fleetRead([settled, machine(tower, null, 'offline')], null)).toBe('settled');
  });
});
