/**
 * Fixture fleet for the design gallery (`src/dev/DesignGallery.tsx`).
 *
 * A design proposal is only reviewable at the size where it hurts: three paired
 * machines, one of them unreachable, sessions of the same project living on two
 * different gateways, and unread work parked on the machine you are NOT looking
 * at. Every number here is deliberate — the fixture is the argument.
 *
 * This file is DEV-ONLY: nothing outside `src/dev` may import it, so it never
 * reaches a store build.
 */

export interface FleetMachine {
  /** The gateway instance id `/healthz` reports — the same one push alerts carry. */
  id: string;
  label: string;
  state: 'online' | 'offline' | 'auth';
  /** Round-trip of the last health probe, shown only when online. */
  latencyMs?: number;
}

export interface FleetSession {
  id: string;
  title: string;
  /** Project name, as the sessions list groups by. */
  project: string;
  root: string;
  machineId: string;
  status: 'LIVE' | 'IDLE' | 'WAITING';
  turns: number;
  /** Turns that finished while the session was closed. */
  unread: number;
  /** Pre-rendered relative time; the gallery has no clock to be honest about. */
  ago: string;
}

export const MACHINES: FleetMachine[] = [
  { id: 'a1b2c3d4e5f60718', label: 'studio-mbp', state: 'online', latencyMs: 8 },
  { id: '99aa88bb77cc66dd', label: 'tower', state: 'online', latencyMs: 12 },
  { id: '0f1e2d3c4b5a6978', label: 'vps-eu', state: 'offline' },
];

export const SESSIONS: FleetSession[] = [
  {
    id: 'sess-7f21c4',
    title: 'multi-gateway sessions',
    project: 'vis',
    root: '~/vis',
    machineId: 'a1b2c3d4e5f60718',
    status: 'LIVE',
    turns: 24,
    unread: 0,
    ago: '2m',
  },
  {
    id: 'sess-3ab991',
    title: 'push intent resolves by gateway id',
    project: 'vis',
    root: '~/vis',
    machineId: '99aa88bb77cc66dd',
    status: 'IDLE',
    turns: 61,
    unread: 3,
    ago: '1h',
  },
  {
    id: 'sess-c14d02',
    title: 'tui transient band chrome',
    project: 'vis',
    root: '~/vis',
    machineId: 'a1b2c3d4e5f60718',
    status: 'WAITING',
    turns: 12,
    unread: 1,
    ago: '18m',
  },
  {
    id: 'sess-88e130',
    title: 'ingress rollout',
    project: 'infrastructure',
    root: '~/infrastructure',
    machineId: '99aa88bb77cc66dd',
    status: 'LIVE',
    turns: 9,
    unread: 0,
    ago: '12m',
  },
  {
    id: 'sess-5d7710',
    title: 'grammar rev bump',
    project: 'tree-sitter-clojure',
    root: '~/tree-sitter-clojure',
    machineId: 'a1b2c3d4e5f60718',
    status: 'IDLE',
    turns: 4,
    unread: 0,
    ago: '3h',
  },
  {
    id: 'sess-2c9f45',
    title: 'nightly benchmark sweep',
    project: 'fff',
    root: '~/fff',
    machineId: '0f1e2d3c4b5a6978',
    status: 'IDLE',
    turns: 31,
    unread: 0,
    ago: '2d',
  },
];

export const machineById = (id: string): FleetMachine =>
  MACHINES.find((machine) => machine.id === id) ?? MACHINES[0];

export const sessionsOf = (machineId: string): FleetSession[] =>
  SESSIONS.filter((session) => session.machineId === machineId);

export const liveCount = (sessions: FleetSession[]): number =>
  sessions.filter((session) => session.status === 'LIVE').length;

export const unreadCount = (sessions: FleetSession[]): number =>
  sessions.reduce((total, session) => total + session.unread, 0);

/** Project name -> its sessions, in fixture order (the list's own order). */
export const byProject = (sessions: FleetSession[]): [string, FleetSession[]][] => {
  const groups = new Map<string, FleetSession[]>();
  for (const session of sessions) {
    const bucket = groups.get(session.project);
    if (bucket) bucket.push(session);
    else groups.set(session.project, [session]);
  }
  return [...groups.entries()];
};

export const projectRoot = (sessions: FleetSession[]): string => sessions[0]?.root ?? '';
