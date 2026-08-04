import { describe, expect, it } from 'vitest';
import {
  creatableMachines,
  dirtyFirst,
  fleetError,
  isFleetLoaded,
  machineCounts,
  machineKey,
  machineLabel,
  newSessionTarget,
  reconcileMachines,
  scopedMachines,
  scopeError,
  scopedConns,
  searchTally,
  scopedSessions,
  sessionIsEmpty,
  sessionIsListed,
  showsScopeStrip,
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
    expect(sessionIsListed(fresh(), true)).toBe(true);
    expect(sessionIsListed(fresh(), false)).toBe(false);
    expect(sessionIsListed(fresh({ title: 'Named' }), false)).toBe(true);
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
// empty — bottom of every timestamp order there is. So it is pinned to the top
// of the list instead, or you never see it again.
describe('dirtyFirst', () => {
  it('floats the rows holding unsent work, keeping the gateway order inside each half', () => {
    const rows = [session('a'), session('b'), session('c'), session('d')];
    const holding = new Set(['b', 'd']);
    expect(dirtyFirst(rows, (row) => holding.has(row.id)).map((row) => row.id)).toEqual([
      'b',
      'd',
      'a',
      'c',
    ]);
  });

  it('leaves a list with nothing unsent exactly as it came', () => {
    const rows = [session('a'), session('b')];
    expect(dirtyFirst(rows, () => false)).toBe(rows);
  });
});
