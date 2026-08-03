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

/**
 * DIRTY: empty on the gateway, but this device is still holding words typed
 * into its composer. Hiding it as "empty" stranded them — the session was gone
 * from the list, so there was no way back to what you wrote and no way to
 * delete the session holding it. A dirty row is listed, badged and openable.
 */
export function sessionIsDirty(session: Session, hasDraftMessage: boolean): boolean {
  return hasDraftMessage && sessionIsEmpty(session);
}

/**
 * Rows the list paints. An empty session earns its place only by being dirty:
 * that is what keeps abandoned taps out of the list without eating unsent work.
 */
export function sessionIsListed(session: Session, hasDraftMessage: boolean): boolean {
  return !sessionIsEmpty(session) || hasDraftMessage;
}
