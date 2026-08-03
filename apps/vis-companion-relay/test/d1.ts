/**
 * A D1 binding backed by `node:sqlite`, so the tests run the REAL SQL of
 * `schema.sql` and `src/store.ts` without workerd, miniflare or a network.
 * Only the surface `src/store.ts` actually uses is implemented.
 */

import { readFileSync } from "node:fs";
import { DatabaseSync } from "node:sqlite";

type Row = Record<string, unknown>;

export function openTestDb(): D1Database {
  const db = new DatabaseSync(":memory:");
  const schema = readFileSync(new URL("../schema.sql", import.meta.url).pathname, "utf8");
  db.exec(schema);

  const prepare = (sql: string) => {
    const bindAndRun = (params: unknown[]) => {
      const statement = db.prepare(sql);
      const args = params as never[];
      return {
        first: async <T>(): Promise<T | null> => (statement.get(...args) as T) ?? null,
        all: async <T>(): Promise<{ results: T[] }> => ({
          results: statement.all(...args) as T[],
        }),
        run: async () => {
          const result = statement.run(...args);
          return { meta: { changes: Number(result.changes) } };
        },
      };
    };
    return { bind: (...params: unknown[]) => bindAndRun(params), ...bindAndRun([]) };
  };

  return {
    prepare,
    /** Test-only escape hatch: read a row back without going through D1 types. */
    query: (sql: string, ...params: unknown[]): Row[] =>
      db.prepare(sql).all(...(params as never[])) as Row[],
  } as unknown as D1Database & { query: (sql: string, ...params: unknown[]) => Row[] };
}

export type TestDb = D1Database & { query: (sql: string, ...params: unknown[]) => Row[] };
