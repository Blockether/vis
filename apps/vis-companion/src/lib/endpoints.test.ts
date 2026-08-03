import { describe, expect, it } from 'vitest';

import { normalizeGatewayUrl } from './endpoints';

describe('normalizeGatewayUrl', () => {
  it('supplies the scheme a human leaves out', () => {
    // The connect form used to reject these outright, which is the one thing a
    // person types after reading a hostname off the machine.
    expect(normalizeGatewayUrl('gateway.example.com')).toBe('https://gateway.example.com');
    expect(normalizeGatewayUrl('  gateway.example.com/  ')).toBe('https://gateway.example.com');
  });

  it('keeps private addresses on plain HTTP, where no certificate exists', () => {
    expect(normalizeGatewayUrl('10.0.0.5:7890')).toBe('http://10.0.0.5:7890');
    expect(normalizeGatewayUrl('127.0.0.1:7890')).toBe('http://127.0.0.1:7890');
    expect(normalizeGatewayUrl('localhost:7890')).toBe('http://localhost:7890');
    expect(normalizeGatewayUrl('my-mac.local:7890')).toBe('http://my-mac.local:7890');
    expect(normalizeGatewayUrl('my-mac.local')).toBe('http://my-mac.local');
    expect(normalizeGatewayUrl('[::1]:7890')).toBe('http://[::1]:7890');
  });

  it('treats an explicit port as a direct gateway, not a tunnel', () => {
    expect(normalizeGatewayUrl('gateway.example.com:7890')).toBe('http://gateway.example.com:7890');
  });

  it('never rewrites a scheme the user typed', () => {
    expect(normalizeGatewayUrl('http://gateway.example.com')).toBe('http://gateway.example.com');
    expect(normalizeGatewayUrl('https://10.0.0.5:7890')).toBe('https://10.0.0.5:7890');
    expect(normalizeGatewayUrl('https://gateway.example.com/base')).toBe(
      'https://gateway.example.com/base',
    );
  });

  it('rejects what cannot be a gateway address', () => {
    expect(normalizeGatewayUrl('')).toBeNull();
    expect(normalizeGatewayUrl('   ')).toBeNull();
    expect(normalizeGatewayUrl('two words')).toBeNull();
    expect(normalizeGatewayUrl('vis://gateway?url=http%3A%2F%2F10.0.0.5%3A7890')).toBeNull();
    expect(normalizeGatewayUrl('ftp://10.0.0.5')).toBeNull();
  });
});
