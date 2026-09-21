// Regression, user report ("it disconnects me all the time, I get Reconnecting
// every few moments"): a large part of that traffic was the app asking
// addresses that had not answered once in fourteen hours. A phone's diagnostics
// export held over eight hundred such attempts — a LAN address from another
// network, the tailnet address of a machine that was re-imaged — and every one
// of them held a socket for a full request timeout while the live streams
// queued behind it.
import { beforeEach, describe, expect, it } from 'vitest';

import {
  forgetUnreachableAddresses,
  isProbeDue,
  noteReachable,
  noteUnreachable,
} from './reachability';

const GONE = 'http://192.168.0.31:7890';
const HERE = 'http://100.109.18.77:7890';
const MINUTE = 60_000;

beforeEach(() => {
  forgetUnreachableAddresses();
});

describe('asking an address that went silent', () => {
  it('asks any address the app has never heard about', () => {
    expect(isProbeDue(GONE)).toBe(true);
  });

  it('lets it rest for a minute after the first silence', () => {
    const now = Date.now();
    noteUnreachable(GONE, now);
    expect(isProbeDue(GONE, now + 59_000)).toBe(false);
    expect(isProbeDue(GONE, now + MINUTE)).toBe(true);
  });

  it('waits longer the longer it stays silent', () => {
    const now = Date.now();
    noteUnreachable(GONE, now);
    noteUnreachable(GONE, now + MINUTE);
    expect(isProbeDue(GONE, now + 2 * MINUTE)).toBe(false);
    expect(isProbeDue(GONE, now + 3 * MINUTE)).toBe(true);
  });

  it('never rests longer than half an hour, so a machine can come back', () => {
    const now = Date.now();
    for (let miss = 0; miss < 20; miss += 1) noteUnreachable(GONE, now);
    expect(isProbeDue(GONE, now + 30 * MINUTE)).toBe(true);
  });

  it('holds this against one address, not against the machine next to it', () => {
    const now = Date.now();
    noteUnreachable(GONE, now);
    expect(isProbeDue(HERE, now)).toBe(true);
  });

  it('is an ordinary address again the moment it answers', () => {
    const now = Date.now();
    noteUnreachable(GONE, now);
    noteReachable(GONE);
    expect(isProbeDue(GONE, now)).toBe(true);
  });

  it('knows the same address written two ways', () => {
    const now = Date.now();
    noteUnreachable('http://192.168.0.31:7890/', now);
    expect(isProbeDue('http://192.168.0.31:7890', now)).toBe(false);
  });

  it('starts over when the device joins a different network', () => {
    const now = Date.now();
    noteUnreachable(GONE, now);
    forgetUnreachableAddresses();
    expect(isProbeDue(GONE, now)).toBe(true);
  });
});
