import { mkdirSync, readdirSync, rmSync, statSync, writeFileSync } from 'node:fs';
import { availableParallelism, tmpdir } from 'node:os';
import path from 'node:path';
import { setTimeout as sleep } from 'node:timers/promises';

// One worker budget for every `vitest run` of this app on the machine, drafts
// included: each run records the workers it uses in a shared temporary directory,
// takes what the other runs leave and waits while they leave too little.

const LEASE = /^(\d+)-(\d+)\.lease$/;
// No single run takes this long; an older lease belongs to a run that is gone.
const STALE_MS = 30 * 60_000;

export interface LeaseOptions {
  dir?: string;
  budget?: number;
  /** The fewest workers worth starting with; below it the run waits for more. */
  minimum?: number;
  pollMs?: number;
  /** After this long the run starts with whatever is free, at least one worker. */
  maxWaitMs?: number;
  pid?: number;
  onWait?: (held: number, budget: number) => void;
}

export interface WorkerLease {
  workers: number;
  release: () => void;
}

function alive(pid: number): boolean {
  try {
    process.kill(pid, 0);
    return true;
  } catch (error) {
    return (error as NodeJS.ErrnoException).code === 'EPERM';
  }
}

/** Workers held by other live runs; the leases of runs that ended are removed. */
function heldByOthers(dir: string, pid: number): number {
  let held = 0;
  for (const name of readdirSync(dir)) {
    const lease = LEASE.exec(name);
    if (!lease || Number(lease[1]) === pid) continue;
    const file = path.join(dir, name);
    const stat = statSync(file, { throwIfNoEntry: false });
    if (!stat) continue;
    if (!alive(Number(lease[1])) || Date.now() - stat.mtimeMs > STALE_MS) {
      rmSync(file, { force: true });
      continue;
    }
    held += Number(lease[2]);
  }
  return held;
}

/** Runs `claim` under the directory's lock, so two runs never count the same free workers. */
async function locked<T>(dir: string, claim: () => T): Promise<T> {
  const lock = path.join(dir, 'lock');
  for (;;) {
    try {
      mkdirSync(lock);
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code !== 'EEXIST') throw error;
      // The lock is held for milliseconds; an old one was left by a killed run.
      const stat = statSync(lock, { throwIfNoEntry: false });
      if (stat && Date.now() - stat.mtimeMs > 5_000) rmSync(lock, { recursive: true, force: true });
      await sleep(20);
      continue;
    }
    try {
      return claim();
    } finally {
      rmSync(lock, { recursive: true, force: true });
    }
  }
}

export async function leaseWorkers({
  dir = path.join(tmpdir(), 'vis-companion-test-workers'),
  budget = Math.max(1, availableParallelism() - 1),
  minimum = Math.ceil(budget / 2),
  pollMs = 250,
  maxWaitMs = 10 * 60_000,
  pid = process.pid,
  onWait,
}: LeaseOptions = {}): Promise<WorkerLease> {
  mkdirSync(dir, { recursive: true });
  const deadline = Date.now() + maxWaitMs;
  let waiting = false;
  for (;;) {
    const { workers, held } = await locked(dir, () => {
      const held = heldByOthers(dir, pid);
      const free = budget - held;
      if (free < Math.min(minimum, budget) && Date.now() < deadline) return { workers: 0, held };
      const workers = Math.max(1, free);
      writeFileSync(path.join(dir, `${pid}-${workers}.lease`), '');
      return { workers, held };
    });
    if (workers > 0) {
      const file = path.join(dir, `${pid}-${workers}.lease`);
      return { workers, release: () => rmSync(file, { force: true }) };
    }
    if (!waiting) onWait?.(held, budget);
    waiting = true;
    await sleep(pollMs);
  }
}

const shared = globalThis as { visTestWorkers?: Promise<number> };

/**
 * `maxWorkers` for this Vitest process. An explicit worker count and interactive
 * watch mode keep Vitest's own choice; a one-off run holds its lease until it exits.
 */
export async function testWorkers(
  argv = process.argv.slice(2),
  env = process.env,
  interactive = Boolean(process.stdin.isTTY),
): Promise<number | undefined> {
  if (env.VITEST_MAX_WORKERS || argv.some((arg) => /^--max-?workers/i.test(arg))) return undefined;
  const once = argv.includes('run') || argv.includes('--run') || Boolean(env.CI) || !interactive;
  if (!once) return undefined;
  // Vitest loads this config again for each project that extends it; lease only once.
  shared.visTestWorkers ??= leaseWorkers({
    onWait: (held, budget) =>
      console.warn(`Waiting for test workers: other runs on this machine hold ${held} of ${budget}.`),
  }).then((lease) => {
    process.once('exit', lease.release);
    return lease.workers;
  });
  return shared.visTestWorkers;
}
