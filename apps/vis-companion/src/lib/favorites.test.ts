// A star is the one piece of list ordering the human typed in themselves, so it
// has to survive a restart and it has to be RANKED. These pin the rank: a clock
// would let two stars tapped in the same millisecond tie, and a tie is exactly
// how "the order is deterministic however many favorites I add" dies.

import { beforeEach, describe, expect, it, vi } from 'vitest';

// `vi.mock` factories run at import time, before module-scope `const`s of this
// file exist — the shared state has to be hoisted with them.
const native = vi.hoisted(() => ({ store: new Map<string, string>() }));

vi.mock('@capacitor/preferences', () => ({
  Preferences: {
    get: async ({ key }: { key: string }) => ({ value: native.store.get(key) ?? null }),
    set: async ({ key, value }: { key: string; value: string }) => {
      native.store.set(key, value);
    },
  },
}));

const STUDIO = 'http://studio.local:7890';
const TOWER = 'http://tower.local:7890';

/** A fresh module, i.e. a restart: the store lives in module scope. */
const restart = async (): Promise<typeof import('./favorites')> => {
  vi.resetModules();
  return import('./favorites');
};

const FAVORITES_KEY = 'vis.sessionFavorites';

/** Whatever storage `bridged` reaches for in this runner, both mirrors hold it. */
const seed = (value: string | null): void => {
  if (value === null) native.store.delete(FAVORITES_KEY);
  else native.store.set(FAVORITES_KEY, value);
  try {
    if (value === null) globalThis.localStorage?.removeItem(FAVORITES_KEY);
    else globalThis.localStorage?.setItem(FAVORITES_KEY, value);
  } catch {
    // No web storage in this runner; the plugin mirror above is enough.
  }
};

/** Let the write-through reach both mirrors before the next module reads them. */
const settled = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));

beforeEach(() => {
  seed(null);
});

describe('favoriteKey', () => {
  it('scopes a star to its gateway: the same sid on two machines is two threads', async () => {
    const { favoriteKey } = await restart();
    expect(favoriteKey(STUDIO, 'abc')).not.toBe(favoriteKey(TOWER, 'abc'));
  });
});

describe('toggleFavorite', () => {
  it('ranks each new star below the ones already there, and answers the new state', async () => {
    const { favoriteKey, favoriteRank, toggleFavorite } = await restart();
    expect(toggleFavorite(favoriteKey(STUDIO, 'a'))).toBe(true);
    expect(toggleFavorite(favoriteKey(STUDIO, 'b'))).toBe(true);
    expect(toggleFavorite(favoriteKey(STUDIO, 'c'))).toBe(true);
    expect(favoriteRank(favoriteKey(STUDIO, 'a'))).toBe(1);
    expect(favoriteRank(favoriteKey(STUDIO, 'b'))).toBe(2);
    expect(favoriteRank(favoriteKey(STUDIO, 'c'))).toBe(3);
  });

  it('sends a re-starred session to the BOTTOM instead of back to its old place', async () => {
    const { favoriteKey, favoriteRank, toggleFavorite } = await restart();
    const a = favoriteKey(STUDIO, 'a');
    const b = favoriteKey(STUDIO, 'b');
    toggleFavorite(a);
    toggleFavorite(b);
    expect(toggleFavorite(a)).toBe(false);
    expect(favoriteRank(a)).toBe(null);
    toggleFavorite(a);
    expect(favoriteRank(a)).toBe(3);
    expect(favoriteRank(b)).toBe(2);
  });
});

describe('forgetFavorites', () => {
  it('drops the stars of deleted sessions and leaves the rest ranked as they were', async () => {
    const { favoriteKey, favoriteRank, forgetFavorites, toggleFavorite } = await restart();
    toggleFavorite(favoriteKey(STUDIO, 'a'));
    toggleFavorite(favoriteKey(STUDIO, 'b'));
    forgetFavorites([favoriteKey(STUDIO, 'a'), favoriteKey(STUDIO, 'gone')]);
    expect(favoriteRank(favoriteKey(STUDIO, 'a'))).toBe(null);
    expect(favoriteRank(favoriteKey(STUDIO, 'b'))).toBe(2);
  });
});

describe('hydrateFavorites', () => {
  it('keeps the stars, and their order, across a restart', async () => {
    const first = await restart();
    await first.hydrateFavorites();
    first.toggleFavorite(first.favoriteKey(STUDIO, 'a'));
    first.toggleFavorite(first.favoriteKey(TOWER, 'b'));
    await settled();

    const next = await restart();
    await next.hydrateFavorites();
    expect(next.favoriteRank(next.favoriteKey(STUDIO, 'a'))).toBe(1);
    expect(next.favoriteRank(next.favoriteKey(TOWER, 'b'))).toBe(2);
  });

  it('rebases a star tapped mid-hydration above the stored ones, never onto their rank', async () => {
    const first = await restart();
    await first.hydrateFavorites();
    first.toggleFavorite(first.favoriteKey(STUDIO, 'stored'));
    await settled();

    const next = await restart();
    const tapped = next.favoriteKey(STUDIO, 'tapped');
    next.toggleFavorite(tapped);
    await next.hydrateFavorites();
    expect(next.favoriteRank(next.favoriteKey(STUDIO, 'stored'))).toBe(1);
    expect(next.favoriteRank(tapped)).toBe(2);
  });

  it('survives junk on disk instead of taking the list down with it', async () => {
    seed('{"a": "nope", "b": 2, ');
    const { favoriteRank, favoriteKey, hydrateFavorites } = await restart();
    await hydrateFavorites();
    expect(favoriteRank(favoriteKey(STUDIO, 'a'))).toBe(null);
  });
});
