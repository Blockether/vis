// Settings rows nest at any depth: a row can hold its own rows in `children`.

import type { Toggle } from './types';

/** Every row and the rows under it, in display order. */
export function flattenSettings(rows: readonly Toggle[]): Toggle[] {
  return rows.flatMap((row) => [row, ...flattenSettings(row.children ?? [])]);
}

/** Put `updated` in place of the row with its id, at any depth. The rows under it stay. */
export function replaceSetting(rows: readonly Toggle[], updated: Toggle): Toggle[] {
  return rows.map((row) => {
    const children = row.children && replaceSetting(row.children, updated);
    if (row.id === updated.id) return { ...updated, ...(children ? { children } : {}) };
    return children ? { ...row, children } : row;
  });
}

/** Keep the rows that match and the rows above a match. A matching row keeps all its rows. */
export function filterSettings(rows: readonly Toggle[], matches: (row: Toggle) => boolean): Toggle[] {
  return rows.flatMap((row) => {
    if (matches(row)) return [row];
    const children = filterSettings(row.children ?? [], matches);
    return children.length > 0 ? [{ ...row, children }] : [];
  });
}
