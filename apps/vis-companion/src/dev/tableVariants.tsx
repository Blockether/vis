/**
 * `vis-table` PROPOSALS: the CSV artifact viewer painted in the real app shell.
 *
 * `vis_attach` of a tabular artifact emits a ````vis-table` fence (five header
 * lines then the CSV payload) and `ChatContent` routes it to
 * `components/DataTable`. These states exist to falsify that component: a wide
 * grid that must scroll instead of squashing, a tall one that must keep its head
 * sticky, and a one-cell table where the whole apparatus has to stay cheap.
 *
 * DEV-ONLY: reachable at `#/__design?v=table` while `vite` runs.
 */
import { DataTable } from '../components/DataTable';

/** Build a fence body exactly as `resources/vis-shims/attach.py` emits it. */
function fence(name: string, label: string, csv: string): string {
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

const WIDE_CSV = `run,commit,branch,suite,tests,failures,skipped,duration_s,peak_rss_mb,verdict
4821,6ea932a46,main,channel-tui,360,0,0,41.7,812,pass
4820,1c0b7ad11,main,channel-tui,344,2,1,44.1,809,fail
4819,9d31ff204,feature/vis-table,foundation,1284,0,12,318.5,2140,pass
4818,77aa10c8e,main,foundation,1272,1,12,331.9,2201,fail
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

const FILES: Record<string, string> = {
  default: 'fleet.csv',
  wide: 'ci-runs.csv',
  tall: 'events.csv',
  solo: 'count.csv',
};

const BODIES: Record<string, string> = {
  default: fence(FILES.default, 'sessions per project', FLEET_CSV),
  wide: fence(FILES.wide, 'last five CI runs', WIDE_CSV),
  tall: fence(FILES.tall, 'gateway journal', TALL_CSV),
  solo: fence(FILES.solo, '', SOLO_CSV),
};

/**
 * The fence as a chat message: the same card `ChatContent` draws around a code
 * block, with the shipped `DataTable` inside it.
 */
export function DataTableVariant({ state }: { state: string }) {
  const body = BODIES[state] ?? BODIES.default;
  const file = FILES[state] ?? FILES.default;
  return (
    <section
      aria-label="Attached table"
      className="mx-auto flex h-full min-h-0 w-full max-w-[1400px] flex-col gap-3 p-3 sm:p-6"
    >
      <p className="font-mono text-meta text-dialog-hint">
        vis_attach("{file}") — rendered from the
        ````vis-table fence
      </p>
      <div className="min-h-0 overflow-hidden border border-dialog-edge bg-panel">
        <DataTable body={body} compact={false} frameless />
      </div>
    </section>
  );
}
