// The human's star on a session.
//
// The GATEWAY owns it: `Session.favorite_rank` is the only copy of the mark that
// exists, so what this device paints and what every other client of the same
// machine paints cannot drift apart. The star used to be kept in this device's own
// storage, and that is exactly how one screen came to show a session starred while
// another showed it plain — two states of one fact, with nothing able to settle
// which was true. Nothing here holds state: these are derivations of the row the
// list already has, and a tap is a PATCH whose echoed row is the new truth.
//
// A favorite is a HUMAN's decision, so it outranks every heuristic the list has:
// live, unread, unsent words and the age cutoff all move rows around on their own,
// and a starred row must not move with them. It is pinned to the top of its project
// and it is never collapsed away.
//
// The mark is a RANK, not a boolean: the gateway allocates max + 1, so two stars
// tapped in the same millisecond cannot tie, and a tie is exactly how "the order is
// deterministic however many favorites I add" dies. Ranks are only ever compared,
// never displayed, so gaps left by unstarring are fine.

import type { Session } from './types';

/**
 * Where this session sits in the starred band, or null when it carries no star.
 * The number is an ORDER, not a time: compare it, never render it.
 */
export function favoriteRank(session: Session): number | null {
  const rank = session.favorite_rank;
  return typeof rank === 'number' && Number.isFinite(rank) ? rank : null;
}

/** Whether the human has starred this session. */
export function isFavorite(session: Session): boolean {
  return favoriteRank(session) !== null;
}

/**
 * The rank a star tapped RIGHT NOW earns, so the row can wear its mark in the same
 * commit as the tap instead of a round trip later. The gateway allocates the real
 * one the same way — below every star already there — and its answer replaces this.
 */
export function nextFavoriteRank(sessions: Iterable<Session>): number {
  let max = 0;
  for (const session of sessions) {
    const rank = favoriteRank(session);
    if (rank !== null && rank > max) max = rank;
  }
  return max + 1;
}
