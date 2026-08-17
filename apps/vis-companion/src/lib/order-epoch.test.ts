import { describe, expect, it } from 'vitest';

import { holdOrder } from './order-epoch';

interface Row {
  id: string;
  millis: number;
}

const of = (row: Row) => row;
const ids = (rows: Row[]) => rows.map((row) => row.id);

/** The answer's own order, freshest first. */
const answer = (...rows: [string, number][]): Row[] => rows.map(([id, millis]) => ({ id, millis }));

describe('holdOrder', () => {
  it('adopts the answer when nothing is held yet', () => {
    const rows = answer(['a', 300], ['b', 200], ['c', 100]);
    const first = holdOrder(null, rows, of);
    expect(ids(first.rows)).toEqual(['a', 'b', 'c']);
    expect(first.pending).toEqual([]);
    expect(ids(holdOrder([], rows, of).rows)).toEqual(['a', 'b', 'c']);
  });

  // Regression, user report (paraphrased: "clicking something suddenly makes it
  // the freshest thing and it goes to the top — the list jumps around like mad").
  // A turn landing on another machine reordered the rows under the reading thumb.
  it('keeps the held order when a row the reader is looking at gets fresher', () => {
    const epoch = ['a', 'b', 'c'];
    const promoted = answer(['c', 900], ['a', 300], ['b', 200]);
    const held = holdOrder(epoch, promoted, of);
    expect(ids(held.rows)).toEqual(['a', 'b', 'c']);
    expect(held.pending).toEqual([]);
  });

  it('holds a brand new row back and counts it', () => {
    const held = holdOrder(['a', 'b'], answer(['new', 900], ['a', 300], ['b', 200]), of);
    expect(ids(held.rows)).toEqual(['a', 'b']);
    expect(held.pending).toEqual(['new']);
  });

  it('appends a row deeper than everything held, because that is the next page', () => {
    const held = holdOrder(['a', 'b'], answer(['a', 300], ['b', 200], ['older', 100]), of);
    expect(ids(held.rows)).toEqual(['a', 'b', 'older']);
    expect(held.pending).toEqual([]);
    // The oldest held clock itself is "more of the same list", never a promotion.
    const tied = holdOrder(['a', 'b'], answer(['a', 300], ['b', 200], ['tied', 200]), of);
    expect(ids(tied.rows)).toEqual(['a', 'b', 'tied']);
    expect(tied.pending).toEqual([]);
  });

  it('drops a held row the answer no longer carries', () => {
    const held = holdOrder(['a', 'gone', 'b'], answer(['a', 300], ['b', 200]), of);
    expect(ids(held.rows)).toEqual(['a', 'b']);
    expect(held.pending).toEqual([]);
  });

  it('adopts again once the epoch holds nothing on screen', () => {
    // Every held row is gone: a scope switch, a query, a window the reader
    // deleted. There is no reading position left to protect.
    const held = holdOrder(['a', 'b'], answer(['x', 900], ['y', 800]), of);
    expect(ids(held.rows)).toEqual(['x', 'y']);
    expect(held.pending).toEqual([]);
  });

  it('never serves a row twice and never loses one', () => {
    const rows = answer(['a', 300], ['b', 200], ['new', 900], ['older', 50]);
    const held = holdOrder(['b', 'a'], rows, of);
    expect(ids(held.rows)).toEqual(['b', 'a', 'older']);
    expect(held.pending).toEqual(['new']);
    expect(new Set([...ids(held.rows), ...held.pending]).size).toBe(rows.length);
  });
});
