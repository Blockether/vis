import { describe, expect, it } from 'vitest';
import { parseTalkLink, talkConnection } from './talk';
import type { GatewayConn } from './types';

describe('parseTalkLink', () => {
  it('reads the machine, the session and the nonce', () => {
    expect(
      parseTalkLink('vis://talk?machine=http%3A%2F%2F10.0.0.5%3A7890&session=s1&at=1500'),
    ).toEqual({ machine: 'http://10.0.0.5:7890', session: 's1', nonce: '1500' });
  });

  it('opens a new session when the link names none', () => {
    expect(parseTalkLink('vis:///talk?at=7')).toEqual({ session: 'new', nonce: '7' });
    expect(parseTalkLink('vis://talk')?.session).toBe('new');
  });

  it('leaves other links to their own handlers', () => {
    expect(parseTalkLink('vis://share?text=hi')).toBeNull();
    expect(parseTalkLink('vis://talking?session=s1')).toBeNull();
    expect(parseTalkLink('https://gateway.example.com/talk')).toBeNull();
    expect(parseTalkLink('')).toBeNull();
  });
});

describe('talkConnection', () => {
  const desk: GatewayConn = { url: 'http://10.0.0.5:7890', token: 't', id: 'gw-desk' };
  const laptop: GatewayConn = {
    url: 'http://127.0.0.1:7890',
    token: 't',
    alts: ['https://gateway.example.com'],
  };

  it('finds the machine by address, other address or gateway id', () => {
    const conns = [desk, laptop];
    expect(talkConnection({ machine: 'http://10.0.0.5:7890/', session: 'new', nonce: '1' }, conns, laptop)).toBe(desk);
    expect(talkConnection({ machine: 'https://gateway.example.com', session: 'new', nonce: '1' }, conns, desk)).toBe(laptop);
    expect(talkConnection({ machine: 'gw-desk', session: 'new', nonce: '1' }, conns, laptop)).toBe(desk);
  });

  it('uses the fallback for an unknown or missing machine', () => {
    expect(talkConnection({ machine: 'http://10.0.0.9:1', session: 'new', nonce: '1' }, [desk, laptop], laptop)).toBe(laptop);
    expect(talkConnection({ session: 'new', nonce: '1' }, [desk, laptop], null)).toBe(desk);
    expect(talkConnection({ session: 'new', nonce: '1' }, [], null)).toBeNull();
  });
});
