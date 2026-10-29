// The star is the one piece of list ordering the human typed in themselves, and it
// is the GATEWAY that keeps it: `favorite_rank` on the session row is the only copy
// there is. It used to live in this device's storage, which is how one screen came
// to show a session starred while another showed it plain — two states of one fact,
// with nothing able to settle which was true. These pin the derivations: a RANK,
// because a clock would let two stars tapped in the same millisecond tie, and a tie
// is exactly how "the order is deterministic however many favorites I add" dies.

import { describe, expect, it } from 'vitest';

import { favoriteRank, isFavorite, nextFavoriteRank } from './favorites';
import type { Session } from './types';

const session = (id: string, rank?: number | null): Session =>
  ({ id, title: id, favorite_rank: rank }) as Session;

describe('favoriteRank', () => {

  it('answers null for a row with no star, however the gateway says so', () => {
    expect(favoriteRank(session('a'))).toBe(null);
    expect(favoriteRank(session('a', null))).toBe(null);
    expect(isFavorite(session('a', null))).toBe(false);
  });

  it('refuses a rank that is not a finite number instead of ordering by garbage', () => {
    expect(favoriteRank({ id: 'a', favorite_rank: 'later' } as unknown as Session)).toBe(null);
    expect(favoriteRank(session('a', Number.NaN))).toBe(null);
  });
});

describe('nextFavoriteRank', () => {
  it('lands a fresh star BELOW every star already there', () => {
    expect(nextFavoriteRank([session('a', 1), session('b', 2), session('c')])).toBe(3);
  });

  it('reads the gaps unstarring leaves as order, not as free slots', () => {
    expect(nextFavoriteRank([session('a', 1), session('b', 7)])).toBe(8);
  });
});
