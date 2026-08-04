/**
 * `vis-table` in the REAL app shell: the shipped `components/DataTable`, not a
 * copy of it. A proposal that renders its own grid can quietly disagree with the
 * component it claims to describe, so every state here drives the shipped one
 * through the same DOM events a finger would: click a head to sort, a gutter
 * number to select, a cell to inspect.
 *
 * The states exist to FALSIFY the grid: a wide sheet that must scroll instead of
 * squashing, a tall one that must page and keep its head, a one-cell table where
 * the whole apparatus has to stay cheap, and a 4 KB JSON cell that must truncate
 * into the inspector instead of stretching the table to 4000 px.
 *
 * DEV-ONLY: reachable at `#/__design?v=table` while `vite` runs.
 */
import { useEffect, useRef } from 'react';
import { DataTable } from '../components/DataTable';

/** Build a fence body exactly as `resources/vis-shims/attach.py` emits it. */
export function fence(name: string, label: string, csv: string): string {
  const grid = csv.trim().split('\n');
  const cols = (grid[0] ?? '').split(',').length;
  const rows = grid.length - 1;
  const bytes = new TextEncoder().encode(csv.trim()).length;
  const size = bytes < 1024 ? `${bytes} B` : `${(bytes / 1024).toFixed(1)} KB`;
  return [
    `[Table: ${name} ${rows} ${rows === 1 ? 'row' : 'rows'} × ${cols} cols, ${size}]${
      label ? ` ${label}` : ''
    }`,
    name,
    'text/csv',
    `${cols}x${rows}`,
    size,
    csv.trim(),
  ].join('\n');
}

const FLEET_CSV = `machine,project,sessions,unread,last seen
studio,vis,12,3,2m ago
studio,spel,4,0,1h ago
mini,vis,9,1,just now
mini,clj-imaging,2,0,3d ago
rack-01,infrastructure,120,17,5m ago
rack-01,vis,0,0,never
`;

/** Ten columns AND blanks: sideways scroll plus `NULL` in one screenshot. */
const WIDE_CSV = `run,commit,branch,suite,tests,failures,skipped,duration_s,peak_rss_mb,verdict
4821,6ea932a46,main,channel-tui,360,0,0,41.7,812,pass
4820,1c0b7ad11,main,channel-tui,344,2,1,44.1,809,fail
4819,9d31ff204,feature/vis-table,foundation,1284,0,12,318.5,2140,pass
4818,77aa10c8e,main,foundation,1272,1,12,,,timeout
4817,0be44d190,main,companion,20,0,0,3.4,268,pass
`;

const TALL_CSV = [
  'ts,level,session,event,ms',
  ...Array.from({ length: 60 }, (_, i) => {
    const level = i % 17 === 0 ? 'WARN' : i % 5 === 0 ? 'DEBUG' : 'INFO';
    return `2026-01-${String((i % 28) + 1).padStart(2, '0')}T09:${String(i % 60).padStart(
      2,
      '0',
    )}:11Z,${level},s-${1000 + i},append-event,${(i * 7) % 913}`;
  }),
].join('\n');

const SOLO_CSV = `total\n1\n`;

/** One cell far wider than any column may be: the inspector's whole reason. */
const BLOB_CSV = `id,status,payload
1,ok,"{""session"":""s-1000"",""tool"":""grep"",""args"":{""query"":[""filter"",""Filter""],""paths"":[""apps/vis-companion/src""],""include"":[""**/*.tsx""]},""hits"":41,""ms"":118}"
2,error,"{""session"":""s-1001"",""tool"":""shell"",""args"":{""commands"":[""npm run build""]},""exit"":1,""stderr"":""vite: command not found""}"
3,ok,
`;

const SHEETS: Record<string, string> = {
  default: fence('fleet.csv', 'sessions per project', FLEET_CSV),
  sorted: fence('fleet.csv', 'sessions per project', FLEET_CSV),
  rows: fence('fleet.csv', 'sessions per project', FLEET_CSV),
  cell: fence('fleet.csv', 'sessions per project', FLEET_CSV),
  blob: fence('tool-calls.csv', 'raw tool payloads', BLOB_CSV),
  wide: fence('ci-runs.csv', 'last five CI runs', WIDE_CSV),
  tall: fence('events.csv', 'gateway journal', TALL_CSV),
  solo: fence('count.csv', '', SOLO_CSV),
};

const CAPTIONS: Record<string, string> = {
  default: 'vis_attach("fleet.csv") — six rows, no pager, no lake',
  sorted: 'sessions ▼ — a third click on the head clears the sort',
  rows: 'the # gutter owns selection: Copy CSV became Copy 3 rows',
  cell: 'a cell click focuses the CELL, not the row',
  blob: 'one 4 KB payload: truncated in the grid, whole in the inspector',
  wide: 'ten columns: it scrolls sideways instead of squashing',
  tall: '60 rows: sticky head, paged, PgUp/PgDn',
  solo: 'one cell: the apparatus stays cheap',
};

/** Click a node the way a finger would, so only shipped handlers can react. */
const click = (node: Element | null | undefined) => (node as HTMLElement | undefined)?.click();

const DRIVERS: Record<string, (root: HTMLElement) => void> = {
  // Head buttons: index 0 is the gutter's select-all, so column i is button i+1.
  sorted: (root) => {
    const head = root.querySelectorAll('th button');
    click(head[3]);
    click(head[3]);
  },
  rows: (root) => {
    const gutter = root.querySelectorAll('td button[aria-pressed]');
    [0, 2, 4].forEach((i) => click(gutter[i]));
  },
  cell: (root) => click(root.querySelectorAll('tbody tr')[4]?.querySelectorAll('td')[2]),
  blob: (root) => click(root.querySelectorAll('tbody tr')[0]?.querySelectorAll('td')[3]),
};

/**
 * The fence as a chat message: the same card `ChatContent` draws around a code
 * block, with the shipped `DataTable` inside it.
 */
export function DataTableVariant({ state }: { state: string }) {
  const body = SHEETS[state] ?? SHEETS.default;
  const caption = CAPTIONS[state] ?? CAPTIONS.default;
  const host = useRef<HTMLDivElement | null>(null);
  const droven = useRef<string | null>(null);

  useEffect(() => {
    // StrictMode runs an effect twice; a TOGGLE driven twice is a driver that
    // did nothing (three selected rows came out unselected, desc came out asc).
    if (droven.current === state) return;
    droven.current = state;
    const root = host.current;
    const drive = DRIVERS[state];
    if (root && drive) drive(root);
  }, [state, body]);

  return (
    <section
      aria-label="Attached table"
      className="mx-auto flex h-full min-h-0 w-full max-w-[1400px] flex-col gap-3 p-3 sm:p-6"
    >
      <p className="shrink-0 font-mono text-meta text-dialog-hint">{caption}</p>
      <div
        ref={host}
        className="min-h-0 w-fit max-w-full overflow-hidden border border-dialog-edge bg-panel"
      >
        <DataTable body={body} compact={false} frameless />
      </div>
    </section>
  );
}
