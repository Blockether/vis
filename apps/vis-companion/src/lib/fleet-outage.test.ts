// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from 'vitest';

import { clearMachineOutage, machineOutage, rememberMachineOutage } from './fleet-outage';

const TOWER = 'http://tower.example.com:4577';
const VPS = 'http://vps.example.com:4577';

beforeEach(() => {
  localStorage.clear();
  vi.useRealTimers();
});

// Regression, user report (paraphrased: "the gateway machine has not been running for hours
// and the session list still shows it as active — you are not saving it anywhere"): the dark
// verdict was a module-level Map, so it died with the JavaScript context the OS kills every
// time the app goes to the background.
describe('what this device found dark', () => {
  it('answers nothing about a machine it has never found dark', () => {
    expect(machineOutage(TOWER)).toBeNull();
  });

  it('outlives the run that measured it', async () => {
    rememberMachineOutage(TOWER, 'Failed to fetch');

    // The relaunch: every module is built again from nothing, and the only thing that can
    // still say what this device learned is what it wrote down.
    vi.resetModules();
    const relaunched = await import('./fleet-outage');
    expect(relaunched.machineOutage(TOWER)).toBe('Failed to fetch');
    expect(relaunched.machineOutage(VPS)).toBeNull();
  });

  it('is cleared by the machine speaking, and stays cleared', async () => {
    rememberMachineOutage(TOWER, 'Failed to fetch');
    clearMachineOutage(TOWER);

    vi.resetModules();
    const relaunched = await import('./fleet-outage');
    expect(relaunched.machineOutage(TOWER)).toBeNull();
  });

  it('keeps the transport\'s own reason, per machine', () => {
    rememberMachineOutage(TOWER, 'no answer in 6s');
    rememberMachineOutage(VPS, 'HTTP 502');
    expect(machineOutage(TOWER)).toBe('no answer in 6s');
    expect(machineOutage(VPS)).toBe('HTTP 502');
  });

  // A machine unpaired months ago must not keep a row here for the life of the install.
  it('forgets a verdict nothing has confirmed for a month', () => {
    vi.useFakeTimers();
    vi.setSystemTime(new Date('2026-01-01T00:00:00Z'));
    rememberMachineOutage(TOWER, 'Failed to fetch');

    vi.setSystemTime(new Date('2026-03-01T00:00:00Z'));
    // The sweep runs on the next write, so the stale row leaves with it.
    rememberMachineOutage(VPS, 'Failed to fetch');
    expect(machineOutage(TOWER)).toBeNull();
    expect(machineOutage(VPS)).toBe('Failed to fetch');
  });

  it('survives a store holding something else entirely', () => {
    localStorage.setItem('vis.fleet-outage.v1', 'not json');
    expect(machineOutage(TOWER)).toBeNull();
    rememberMachineOutage(TOWER, 'Failed to fetch');
    expect(machineOutage(TOWER)).toBe('Failed to fetch');
  });
});
