import { spawnSync } from 'node:child_process';
import { existsSync, mkdtempSync, readdirSync, rmSync, writeFileSync } from 'node:fs';
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

  it('starts with one worker when the wait runs out', async () => {
    writeFileSync(path.join(dir, `${process.ppid}-8.lease`), '');
    const lease = await leaseWorkers({ dir, budget: 8, pollMs: 10, maxWaitMs: 30 });
    expect(lease.workers).toBe(1);
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
