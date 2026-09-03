/**
 * Hold visible session IDs in their accepted order while refreshing row content in
 * place. Promotions wait behind an explicit reader action; deeper pages append. Renew
 * the epoch only when the reader changes scope/order, accepts updates, or returns after
 * a real absence.
 */

import { useCallback, useEffect, useMemo, useRef, useState } from 'react';

/** A row an epoch can order: its identity, and the content clock it sorts by. */
export interface EpochRow {
  id: string;
  /** Content time in ms — `sessionMillis`, never a touch clock. */
  millis: number;
}

/** Ids in the order the reader agreed to, freshest first. */
export type OrderEpoch = readonly string[];

export interface HeldRows<T> {
  /** What to paint: the held order, with deeper arrivals appended. */
  rows: T[];
  /** Ids of rows that want to be higher up and are waiting for a trigger. */
  pending: string[];
}

/** No row admitted: the default, and the shape every caller before `admit` had. */
const EMPTY_ADMIT: ReadonlySet<string> = new Set<string>();

/**
 * Apply `epoch` to `rows` (the answer's own order, freshest first).
 *
 * An epoch that holds nothing on screen is not held at all: a first paint, a
 * scope the reader just switched to, or a window whose every row was deleted
 * adopts what it was given, because there is no reading position to protect.
 *
 * `admit` names rows this reader is not surprised by — the session THIS device
 * just started. The pill exists for rows another machine wrote under a still
 * thumb; a row the reader asked for a second ago is not one of those, and making
 * them tap to see what they just created is the complaint that added this
 * argument. An admitted row is spliced into the held order at the position the
 * answer's own order gives it (freshest first, so normally the top) and is never
 * counted as pending.
 */
export function holdOrder<T>(
  epoch: OrderEpoch | null,
  rows: readonly T[],
  of: (row: T) => EpochRow,
  admit: ReadonlySet<string> = EMPTY_ADMIT,
): HeldRows<T> {
  if (!epoch || epoch.length === 0) return { rows: [...rows], pending: [] };

  const byId = new Map<string, T>();
  for (const row of rows) byId.set(of(row).id, row);

  const order: string[] = [];
  for (const id of epoch) {
    if (byId.has(id)) order.push(id);
  }
  if (order.length === 0) return { rows: [...rows], pending: [] };

  // An admitted row joins the held order where the answer would have put it: after
  // the last held row that is above it naturally, so the freshest lands on top.
  if (admit.size > 0) {
    let cursor = 0;
    for (const row of rows) {
      const id = of(row).id;
      const at = order.indexOf(id);
      if (at >= 0) {
        cursor = at + 1;
        continue;
      }
      if (admit.has(id)) {
        order.splice(cursor, 0, id);
        cursor += 1;
      }
    }
  }

  const held: T[] = order.map((id) => byId.get(id) as T);

  // The deepest row the reader has agreed to see. Anything at or below it can
  // only be MORE of the same list, so it appends without moving a thing.
  const deepest = held.reduce((oldest, row) => Math.min(oldest, of(row).millis), Infinity);
  const known = new Set(order);
  const appended: T[] = [];
  const pending: string[] = [];
  for (const row of rows) {
    const { id, millis } = of(row);
    if (known.has(id)) continue;
    if (millis <= deepest) appended.push(row);
    else pending.push(id);
  }
  return { rows: [...held, ...appended], pending };
}

/** A minute away and the list you come back to is not the list you left. */
export const EPOCH_STALE_AWAY_MS = 60_000;

/**
 * Hold the order of one view.
 *
 * `viewKey` names the QUESTION the rows answer (machine scope, the live query):
 * a different question has a different answer and no position worth keeping, so
 * the epoch is taken fresh. `naturalIds` is the current answer's own order,
 * flat, across every section on screen.
 *
 * `isSettled` is the caller saying the rows are worth holding: a fleet whose
 * machines are still answering one by one, or a query still being served, is
 * FILLING IN, and freezing a half-arrived answer would park the machine that
 * answered second behind a pill. Until it is true the answer's own order is
 * painted; the moment it turns true, that order is the epoch.
 *
 * `adopt()` takes the order as of the NEXT commit, not this one: the reader
 * actions that call it (starring a row, tapping the pill) repaint rows in the
 * same tick, and the order worth keeping is the one those rows produce.
 */
export function useOrderEpoch(
  viewKey: string,
  naturalIds: readonly string[],
  isSettled: boolean,
): { epoch: OrderEpoch | null; adopt: () => void } {
  const [held, setHeld] = useState<{ key: string; asked: number; ids: OrderEpoch } | null>(null);
  const [asked, setAsked] = useState(0);
  const adopt = useCallback(() => setAsked((count) => count + 1), []);

  // Declared BEFORE the effect that reads it, so within one commit the order
  // stored is the order this render was handed.
  const latest = useRef(naturalIds);
  useEffect(() => {
    latest.current = naturalIds;
  }, [naturalIds]);

  // An asked-for order is granted in THIS render, not on the commit after it: the
  // star that asked repaints its own row in the same tick, and the group that
  // follows that row to its new page reads the order from a layout effect — a beat
  // earlier than any effect of this hook could answer. So the render a reader
  // asked for is simply unheld, and the effect below records what it painted.
  const asking = held !== null && held.asked !== asked;
  const epoch = isSettled && !asking && held && held.key === viewKey ? held.ids : null;
  const heldIds = useMemo(() => new Set(epoch ?? []), [epoch]);
  // An epoch none of whose rows are on screen holds nothing: renew it rather
  // than leave the view permanently unheld.
  const holdsNothing = epoch !== null && !naturalIds.some((id) => heldIds.has(id));

  useEffect(() => {
    if (!isSettled) return;
    // Three reasons to take an order, and the rows are not one of them: an
    // arriving promotion must never renew the epoch it is waiting behind.
    if (epoch !== null && !holdsNothing) return;
    setHeld({ key: viewKey, asked, ids: latest.current });
  }, [asked, epoch, holdsNothing, isSettled, viewKey]);

  return { epoch, adopt };
}
