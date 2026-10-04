// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from 'vitest';

import {
  clearMachineOutage,
  isMachineHidden,
  machineOutage,
  rememberMachineOutage,
} from './fleet-outage';

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

  it("keeps the transport's own reason, per machine", () => {
    rememberMachineOutage(TOWER, 'no answer in 6s');
    rememberMachineOutage(VPS, 'HTTP 502');
    expect(machineOutage(TOWER)).toBe('no answer in 6s');
    expect(machineOutage(VPS)).toBe('HTTP 502');
  });

  it('keeps an outage until the machine answers, regardless of its age', () => {
    vi.useFakeTimers();
    vi.setSystemTime(new Date('2026-01-01T00:00:00Z'));
    for (let attempt = 0; attempt < 3; attempt += 1)
      rememberMachineOutage(TOWER, 'Failed to fetch');

    vi.setSystemTime(new Date('2027-03-01T00:00:00Z'));
    rememberMachineOutage(VPS, 'Failed to fetch');
    expect(machineOutage(TOWER)).toBe('Failed to fetch');
    expect(isMachineHidden(TOWER)).toBe(true);
    expect(machineOutage(VPS)).toBe('Failed to fetch');
  });

  it('persists consecutive failures across cold starts and hides on the third', async () => {
    expect(rememberMachineOutage(TOWER, 'Failed to fetch')).toBe(1);
    expect(isMachineHidden(TOWER)).toBe(false);
    expect(rememberMachineOutage(TOWER, 'silent for 15s')).toBe(2);
    expect(isMachineHidden(TOWER)).toBe(false);

    vi.resetModules();
    const relaunched = await import('./fleet-outage');
    expect(relaunched.rememberMachineOutage(TOWER, 'Failed to fetch')).toBe(3);
    expect(relaunched.isMachineHidden(TOWER)).toBe(true);
    expect(relaunched.isMachineHidden(VPS)).toBe(false);

    vi.resetModules();
    const reopened = await import('./fleet-outage');
    expect(reopened.isMachineHidden(TOWER)).toBe(true);
    expect(reopened.rememberMachineOutage(TOWER, 'Failed to fetch')).toBe(3);
  });

  it('resets the hiding threshold after recovery, including across cold starts', async () => {
    for (let attempt = 0; attempt < 3; attempt += 1)
      rememberMachineOutage(TOWER, 'Failed to fetch');
    expect(isMachineHidden(TOWER)).toBe(true);
    clearMachineOutage(TOWER);
    expect(isMachineHidden(TOWER)).toBe(false);

    vi.resetModules();
    const relaunched = await import('./fleet-outage');
    expect(relaunched.rememberMachineOutage(TOWER, 'Failed to fetch')).toBe(1);
    expect(relaunched.isMachineHidden(TOWER)).toBe(false);
    expect(relaunched.rememberMachineOutage(TOWER, 'Failed to fetch')).toBe(2);
    expect(relaunched.isMachineHidden(TOWER)).toBe(false);
    expect(relaunched.rememberMachineOutage(TOWER, 'Failed to fetch')).toBe(3);
    expect(relaunched.isMachineHidden(TOWER)).toBe(true);
  });

  it('does not restore a hidden machine when other machines fail', () => {
    for (let attempt = 0; attempt < 3; attempt += 1)
      rememberMachineOutage(TOWER, 'Failed to fetch');
    for (let index = 0; index < 40; index += 1)
      rememberMachineOutage(`http://gateway.example.com:${8000 + index}`, 'Failed to fetch');
    expect(isMachineHidden(TOWER)).toBe(true);
  });

  it('survives a store holding something else entirely', () => {
    localStorage.setItem('vis.fleet-outage.v1', 'not json');
    expect(machineOutage(TOWER)).toBeNull();
    rememberMachineOutage(TOWER, 'Failed to fetch');
    expect(machineOutage(TOWER)).toBe('Failed to fetch');
  });
});
