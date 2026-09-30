import { afterEach, describe, expect, it, vi } from 'vitest';
import { randomUuid } from './uuid';

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

// Regression #299: randomUUID is unavailable on non-loopback HTTP pages.
describe('cryptographic UUIDs', () => {
  it('prefers native randomUUID and preserves its Crypto receiver', () => {
    const expected = '01234567-89ab-4cde-8123-456789abcdef';
    const crypto = {
      randomUUID: vi.fn(function (this: unknown) {
        expect(this).toBe(crypto);
        return expected;
      }),
      getRandomValues: vi.fn(),
    };
    vi.stubGlobal('crypto', crypto);

    expect(randomUuid()).toBe(expected);
    expect(crypto.randomUUID).toHaveBeenCalledExactlyOnceWith();
    expect(crypto.getRandomValues).not.toHaveBeenCalled();
  });

  it.each([
    { bytes: Array(16).fill(0), expected: '00000000-0000-4000-8000-000000000000' },
    { bytes: Array(16).fill(255), expected: 'ffffffff-ffff-4fff-bfff-ffffffffffff' },
    {
      bytes: Array.from({ length: 16 }, (_, index) => index),
      expected: '00010203-0405-4607-8809-0a0b0c0d0e0f',
    },
  ])('formats $expected from cryptographic bytes without randomUUID', ({ bytes, expected }) => {
    const getRandomValues = vi.fn((target: Uint8Array) => {
      target.set(bytes);
      return target;
    });
    vi.stubGlobal('crypto', { getRandomValues });

    expect(randomUuid()).toBe(expected);
    expect(getRandomValues).toHaveBeenCalledOnce();
    expect(getRandomValues.mock.calls[0][0]).toBeInstanceOf(Uint8Array);
    expect(getRandomValues.mock.calls[0][0]).toHaveLength(16);
  });

  it.each([undefined, {}])(
    'fails closed when cryptographic randomness is missing: %s',
    (crypto) => {
      vi.stubGlobal('crypto', crypto);
      const weakRandom = vi.spyOn(Math, 'random');

      expect(randomUuid).toThrow('Cryptographic randomness is unavailable.');
      expect(weakRandom).not.toHaveBeenCalled();
    },
  );

  it('preserves errors from the cryptographic random source', () => {
    const failure = new Error('Random source unavailable');
    vi.stubGlobal('crypto', {
      getRandomValues: vi.fn(() => {
        throw failure;
      }),
    });

    expect(randomUuid).toThrow(failure);
  });
});
