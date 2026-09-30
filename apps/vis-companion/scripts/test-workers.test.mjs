import { spawnSync } from 'node:child_process';
import { existsSync, mkdtempSync, readdirSync, rmSync, utimesSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import { leaseWorkers, testWorkers } from './test-workers.ts';

let dir;
beforeEach(() => {
  dir = mkdtempSync(path.join(tmpdir(), 'test-workers-'));
});
afterEach(() => rmSync(dir, { recursive: true, force: true }));

const leases = () => readdirSync(dir).filter((name) => name.endsWith('.lease'));

describe('leaseWorkers', () => {
  it('takes the whole budget on an idle machine and gives it back', async () => {
    const lease = await leaseWorkers({ dir, budget: 8 });
    expect(lease.workers).toBe(8);
    expect(leases()).toEqual([`${process.pid}-8.lease`]);
    lease.release();
    expect(leases()).toEqual([]);
  });

  it('takes what another run leaves, and waits while that is too little', async () => {
    const other = path.join(dir, `${process.ppid}-6.lease`);
    writeFileSync(other, '');
    const rest = await leaseWorkers({ dir, budget: 8, minimum: 2 });
    expect(rest.workers).toBe(2);
    rest.release();
    const waits = [];
    const pending = leaseWorkers({ dir, budget: 8, pollMs: 10, onWait: (...seen) => waits.push(seen) });
    await expect.poll(() => waits).toEqual([[6, 8]]);
    rmSync(other);
    await expect(pending).resolves.toMatchObject({ workers: 8 });
  });

  it('drops the lease of a run that has ended', async () => {
    const { pid } = spawnSync(process.execPath, ['-e', '']);
    writeFileSync(path.join(dir, `${pid}-8.lease`), '');
    const lease = await leaseWorkers({ dir, budget: 8 });
    expect(lease.workers).toBe(8);
    expect(existsSync(path.join(dir, `${pid}-8.lease`))).toBe(false);
  });

  it('uses a free worker after the preferred minimum wait expires', async () => {
    writeFileSync(path.join(dir, `${process.ppid}-7.lease`), '');
    const lease = await leaseWorkers({ dir, budget: 8, pollMs: 10, maxWaitMs: 30 });
    expect(lease.workers).toBe(1);
    lease.release();
  });

  it.each([2, 3])('keeps waiting after the deadline when %i workers fill a budget of two', async (held) => {
    const other = path.join(dir, `${process.ppid}-${held}.lease`);
    writeFileSync(other, '');
    const waits = [];
    let admitted = false;
    const pending = leaseWorkers({ dir, budget: 2, pollMs: 10, maxWaitMs: 0, onWait: (...seen) => waits.push(seen) })
      .then((lease) => { admitted = true; return lease; });
    try {
      await expect.poll(() => waits).toEqual([[held, 2]]);
      expect(admitted).toBe(false);
      expect(leases()).toEqual([`${process.ppid}-${held}.lease`]);
    } finally {
      rmSync(other, { force: true });
      (await pending).release();
    }
  });

  it('keeps an old lease while its owner is alive', async () => {
    const other = path.join(dir, `${process.ppid}-2.lease`);
    writeFileSync(other, '');
    const old = new Date(Date.now() - 31 * 60_000);
    utimesSync(other, old, old);
    const waits = [];
    const pending = leaseWorkers({ dir, budget: 2, pollMs: 10, maxWaitMs: 0, onWait: (...seen) => waits.push(seen) });
    try {
      await expect.poll(() => waits).toEqual([[2, 2]]);
      expect(existsSync(other)).toBe(true);
      expect(leases()).toEqual([`${process.ppid}-2.lease`]);
    } finally {
      rmSync(other, { force: true });
      (await pending).release();
    }
  });
});

describe('testWorkers', () => {
  it.each([
    ['an explicit count', ['run', '--maxWorkers=2'], {}, false],
    ['VITEST_MAX_WORKERS', ['run'], { VITEST_MAX_WORKERS: '2' }, false],
    ['interactive watch mode', [], {}, true],
  ])('leaves %s to Vitest', async (_label, argv, env, interactive) => {
    await expect(testWorkers(argv, env, interactive)).resolves.toBeUndefined();
  });
});
