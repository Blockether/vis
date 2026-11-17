/**
 * Vis on this computer, for a person who has only the desktop app: the installed
 * engine, its gateway, and the work that the desktop host does for them.
 *
 * Only the desktop host on a platform that has an engine gives this state. In a
 * browser, on a phone, on Windows and on an Intel Mac `engine` stays `null`.
 */
import { useSyncExternalStore } from 'react';
import { desktopInvoke } from './desktop';
import { normalizeAddress } from './endpoints';
import { GatewayClient } from './gateway';
import type { GatewayConn } from './types';

export type EngineTask = 'install' | 'update' | 'start';

/**
 * `log` holds the lines that the task printed, oldest first. `latest` is the newest
 * release that a check found; it is absent before a check.
 */
export type LocalEngine =
  | { kind: 'absent' }
  | { kind: 'working'; task: EngineTask; log: string[] }
  | { kind: 'failed'; task: EngineTask; reason: string; log: string[] }
  | { kind: 'stopped' | 'running'; version: string; track: string; latest?: string };

export interface LocalEngineState {
  engine: LocalEngine | null;
  /** The gateway serves this app, so the app can pair with it. */
  isGatewayOpen: boolean;
  /** The token that the gateway asks for, when it asks for one and this computer has it. */
  gatewayToken: string | null;
  /** The id that the gateway of this computer reports, when it answers. */
  gatewayId: string | null;
  isChecking: boolean;
  /** The last check for a newer release got no answer. */
  isCheckFailed: boolean;
  /** The person asked for Vis on this computer: pair with it beside other machines. */
  wantsPairing: boolean;
}

/** The address of a gateway that the `vis-agent gateway start` command starts. */
export const LOCAL_GATEWAY_URL = 'http://127.0.0.1:7890';

const POLL_MS = 700;
/** A first start prepares the engine, so it can take longer than a later start. */
const GATEWAY_WAIT_MS = 120_000;

interface HostStatus {
  supported: boolean;
  installed: boolean;
  version: string | null;
  track: string;
}
interface HostProgress {
  task: string;
  running: boolean;
  ok: boolean;
  log: string[];
}
/** `locked`: the gateway asks for a token that this computer does not have. */
interface Probe {
  state: 'open' | 'locked' | 'down';
  id: string | null;
  token: string | null;
}

let state: LocalEngineState = {
  engine: null,
  isGatewayOpen: false,
  gatewayToken: null,
  gatewayId: null,
  isChecking: false,
  isCheckFailed: false,
  wantsPairing: false,
};
const listeners = new Set<() => void>();
/** Each verb takes a new turn. The loop of an earlier turn ends when it sees the change. */
let turn = 0;
let latest: string | undefined;
let hasBooted = false;

function set(patch: Partial<LocalEngineState>): void {
  state = { ...state, ...patch };
  for (const listener of listeners) listener();
}

function subscribe(listener: () => void): () => void {
  listeners.add(listener);
  return () => listeners.delete(listener);
}

export const localEngineState = (): LocalEngineState => state;

export function useLocalEngine(): LocalEngineState {
  return useSyncExternalStore(subscribe, localEngineState, localEngineState);
}

const pause = (ms: number) => new Promise<void>((resolve) => setTimeout(resolve, ms));

async function host<T>(command: string, payload?: Record<string, unknown>): Promise<T> {
  const invoke = desktopInvoke();
  if (!invoke) throw new Error('This is not the desktop app.');
  return (await invoke(command, payload)) as T;
}

/**
 * Ask the gateway of this computer if it serves this app. Its health route answers
 * each client, so a gateway that asks for a token must also serve a real request
 * with the token that the host reads on this computer.
 */
async function probeGateway(): Promise<Probe> {
  const health = await new GatewayClient({ url: LOCAL_GATEWAY_URL }).health().catch(() => null);
  if (!health) return { state: 'down', id: null, token: null };
  const id = health.id ?? null;
  if (health.require_token === false) return { state: 'open', id, token: null };
  const token = await host<string | null>('engine_gateway_token').catch(() => null);
  if (!token) return { state: 'locked', id, token: null };
  return new GatewayClient({ url: LOCAL_GATEWAY_URL, token })
    .capabilities(undefined, { force: true })
    .then(
      () => ({ state: 'open' as const, id, token }),
      () => ({ state: 'locked' as const, id, token: null }),
    );
}

/** `a` is a newer version than `b`. The parts compare as numbers: 0.5.9 is older than 0.5.10. */
export function isNewerVersion(a: string, b: string): boolean {
  const parts = (version: string) => version.split(/[.-]/).map((part) => Number.parseInt(part, 10) || 0);
  const [left, right] = [parts(a), parts(b)];
  for (let i = 0; i < Math.max(left.length, right.length); i++) {
    const difference = (left[i] ?? 0) - (right[i] ?? 0);
    if (difference !== 0) return difference > 0;
  }
  return false;
}

function fail(task: EngineTask, reason: string, log: string[]): void {
  set({ engine: { kind: 'failed', task, reason, log } });
}

/** Show what the host and the gateway report now. */
async function settle(mine: number): Promise<void> {
  const [status, probe] = await Promise.all([
    host<HostStatus | null>('engine_status'),
    probeGateway(),
  ]);
  if (mine !== turn) return;
  if (!status?.supported) return set({ engine: null });
  const gateway = {
    isGatewayOpen: probe.state === 'open',
    gatewayToken: probe.token,
    gatewayId: probe.id,
  };
  if (!status.installed || status.version === null) {
    return set({ engine: { kind: 'absent' }, ...gateway });
  }
  set({
    engine: {
      kind: probe.state === 'down' ? 'stopped' : 'running',
      version: status.version,
      track: status.track,
      ...(latest ? { latest } : {}),
    },
    ...gateway,
  });
}

/** Start the gateway when it does not answer, and wait until it answers. */
async function open(mine: number, task: EngineTask, log: string[]): Promise<void> {
  set({ engine: { kind: 'working', task, log } });
  if ((await probeGateway()).state === 'down') {
    await host('engine_gateway_start');
    const deadline = Date.now() + GATEWAY_WAIT_MS;
    while ((await probeGateway()).state === 'down') {
      if (mine !== turn) return;
      if (Date.now() > deadline) {
        return fail('start', 'The gateway did not answer.', await host<string[]>('engine_gateway_log'));
      }
      await pause(POLL_MS);
    }
  }
  if (mine === turn) await settle(mine);
}

/** Follow the task of the host to its end, then start the gateway. */
async function follow(mine: number, task: 'install' | 'update'): Promise<void> {
  for (;;) {
    const progress = await host<HostProgress>('engine_progress');
    if (mine !== turn) return;
    if (!progress.running) {
      if (progress.ok) return open(mine, task, progress.log);
      const name = task === 'install' ? 'installation' : 'update';
      return fail(task, `The ${name} stopped with an error.`, progress.log);
    }
    set({ engine: { kind: 'working', task, log: progress.log } });
    await pause(POLL_MS);
  }
}

async function run(task: EngineTask, work: (mine: number) => Promise<void>): Promise<void> {
  const mine = ++turn;
  set({ engine: { kind: 'working', task, log: [] } });
  try {
    await work(mine);
  } catch (e) {
    if (mine === turn) fail(task, e instanceof Error ? e.message : String(e), []);
  }
}

const start = () => run('start', (mine) => open(mine, 'start', []));

export function installEngine(): Promise<void> {
  set({ wantsPairing: true });
  return run('install', async (mine) => {
    await host('engine_run', { task: 'install' });
    await follow(mine, 'install');
  });
}

/** Update the engine and this app, then start the new gateway. */
export function updateEngine(): Promise<void> {
  return run('update', async (mine) => {
    await host('engine_run', { task: 'update' });
    await follow(mine, 'update');
  });
}

export function startEngine(): Promise<void> {
  set({ wantsPairing: true });
  return start();
}

/** Stop the task in progress, and show what it left on this computer. */
export async function stopEngineTask(): Promise<void> {
  const mine = ++turn;
  try {
    await host('engine_stop');
    await settle(mine);
  } catch (e) {
    if (mine === turn) fail('start', e instanceof Error ? e.message : String(e), []);
  }
}

/** Ask for the newest release. */
export async function checkEngineUpdate(): Promise<void> {
  set({ isChecking: true, isCheckFailed: false });
  const found = await host<string | null>('engine_latest').catch(() => null);
  latest = found ?? latest;
  const engine = state.engine;
  const isInstalled = engine?.kind === 'stopped' || engine?.kind === 'running';
  set({
    isChecking: false,
    isCheckFailed: found === null,
    ...(isInstalled && latest ? { engine: { ...engine, latest } } : {}),
  });
}

/** Pair the app with the gateway of this computer, which runs already. */
export function connectEngine(): void {
  set({ wantsPairing: true });
}

/**
 * A paired machine is the gateway of this computer: it has the address, or it has
 * the id. Another loopback port is another gateway.
 */
export function isLocalPaired(conns: readonly GatewayConn[], gatewayId: string | null): boolean {
  return conns.some(
    (conn) =>
      (gatewayId !== null && conn.id === gatewayId) ||
      [conn.url, ...(conn.alts ?? [])].some((url) => normalizeAddress(url) === LOCAL_GATEWAY_URL),
  );
}

export function clearLocalPairing(): void {
  if (state.wantsPairing) set({ wantsPairing: false });
}

/**
 * Read this computer one time for each page. An installed engine gets its gateway
 * started. A task of the host continues through a reload of the page, so follow it.
 */
export async function bootLocalEngine(): Promise<void> {
  if (hasBooted || !desktopInvoke()) return;
  hasBooted = true;
  // A host without the engine commands refuses the call or gives no answer: no view shows.
  const progress = await host<HostProgress | null>('engine_progress').catch(() => null);
  if (!progress) return;
  const { task } = progress;
  if (progress.running && (task === 'install' || task === 'update')) {
    return run(task, (mine) => follow(mine, task));
  }
  const mine = ++turn;
  await settle(mine).catch(() => {});
  if (mine !== turn) return;
  if (state.engine?.kind === 'stopped') await start();
  const engine = state.engine;
  if (engine?.kind === 'running' && engine.track === 'release') await checkEngineUpdate();
}

/** The verbs in the shape that the `ThisComputer` view takes. */
export const ENGINE_VERBS = {
  onInstall: () => void installEngine(),
  onUpdate: () => void updateEngine(),
  onStart: () => void startEngine(),
  onStop: () => void stopEngineTask(),
  onCheck: () => void checkEngineUpdate(),
};
