/**
 * `vis-table` REDESIGN proposals: the grid rebuilt around the five faults the
 * shipped `DataTable` shots exposed, painted in the real app shell so the
 * viewport IS the proposal.
 *
 * What each proposal has to prove, in the reviewer's language:
 *   1. columns sized to CONTENT — a 5-column CSV no longer smears over 1232 px;
 *   2. a `#` gutter that is both the ordinal and the row-select control, so
 *      "1–25 of 60" is corroborated by the grid instead of asserted by a chip;
 *   3. a header with a real ground (`bg-edge-strong`, not an 8/255 tint), a
 *      2 px rule and an always-visible sort affordance that a third click clears;
 *   4. `NULL` that does not read as an empty string, and a long cell that
 *      truncates into an inspector rail instead of stretching the table;
 *   5. a pager with 44 pt targets, real contrast, `PgUp/PgDn/Home/End` parity
 *      with the TUI, and a `Fit` page size that FILLS the box it lives in.
 *
 * Only the presentation is new: parsing, sorting and paging stay the shipped
 * pure functions from `components/DataTable`, so a proposal cannot quietly
 * disagree with the component it wants to replace.
 *
 * DEV-ONLY: reachable at `#/__design?v=grid-ruled` while `vite` runs.
 */
import { useCallback, useMemo, useRef, useState } from 'react';
import {
  PAGE_SIZES,
  clampPage,
  isNumericColumn,
  pageCount,
  pageRange,
  pageRows,
  parseCsv,
  parseTableBlock,
  sortRows,
  type TableRow,
} from '../components/DataTable';
import { fence } from './tableVariants';

/** One data row, in px: the unit `Fit` divides the free height by. */
const ROW_H = 26;
/** The sticky head, in px. */
const HEAD_H = 30;

type Sort = { index: number; dir: 'asc' | 'desc' } | null;
type Cell = { row: number; col: number } | null;

/**
 * Measures the scroller: how many rows a page must have to FILL it, and whether
 * it scrolls sideways (which is the only honest reason to paint an edge fade).
 */
function useFit(): [{ rows: number; overflowX: boolean }, (node: HTMLDivElement | null) => void] {
  const [fit, setFit] = useState({ rows: 12, overflowX: false });
  const observer = useRef<ResizeObserver | null>(null);
  const ref = useCallback((node: HTMLDivElement | null) => {
    observer.current?.disconnect();
    observer.current = null;
    if (!node) return;
    const measure = () => {
      setFit({
        rows: Math.max(4, Math.floor((node.clientHeight - HEAD_H) / ROW_H)),
        overflowX: node.scrollWidth > node.clientWidth + 1,
      });
    };
    const ro = new ResizeObserver(measure);
    ro.observe(node);
    observer.current = ro;
    measure();
  }, []);
  return [fit, ref];
}

const HEAD_CELL =
  'sticky top-0 z-10 h-[30px] border-b-2 border-code-edge bg-edge-strong px-0 align-middle';
const BODY_CELL = 'h-[26px] border-b border-code-edge p-0';
const PAGER_BUTTON =
  'flex min-h-11 min-w-11 items-center justify-center border border-edge-strong px-3 text-ui text-code-foreground active:bg-hover disabled:opacity-40';

export function ProposedGrid({
  body,
  inspect = null,
  preselect = [],
  sort: initialSort = null,
}: {
  body: string;
  /** Pre-focused cell, so a screenshot can show the inspector doing its job. */
  inspect?: Cell;
  /** Pre-selected row keys, so a screenshot can show gutter multi-select. */
  preselect?: number[];
  /** Pre-applied sort, so a screenshot can show the head in its sorted state. */
  sort?: Sort;
}) {
  const artifact = useMemo(() => parseTableBlock(body), [body]);
  const grid = useMemo(() => parseCsv(artifact.csv), [artifact.csv]);
  const header = grid[0] ?? [];
  const rows = useMemo<TableRow[]>(
    () => grid.slice(1).map((cells, index) => ({ key: index, cells })),
    [grid],
  );
  const aligns = useMemo(
    () =>
      header.map((_, index) =>
        isNumericColumn(
          rows.map((r) => r.cells),
          index,
        ),
      ),
    [header, rows],
  );

  const [sort, setSort] = useState<Sort>(initialSort);
  const [size, setSize] = useState<number | 'fit'>('fit');
  const [page, setPage] = useState(0);
  const [selected, setSelected] = useState<ReadonlySet<number>>(new Set(preselect));
  const [cell, setCell] = useState<Cell>(inspect);
  const [fit, fitRef] = useFit();

  const ordered = useMemo(
    () => (sort ? sortRows(rows, sort.index, sort.dir) : rows),
    [rows, sort],
  );
  const step = size === 'fit' ? fit.rows : size;
  const pages = pageCount(ordered.length, step);
  const current = clampPage(page, ordered.length, step);
  const shown = useMemo(() => pageRows(ordered, current, step), [ordered, current, step]);
  const range = pageRange(ordered.length, current, step);
  /** A sheet nobody would page through must not grow a pager. */
  const paged = ordered.length > PAGE_SIZES[0];

  /** asc → desc → OFF: the file's own order stays reachable. */
  const toggleSort = (index: number) =>
    setSort((currentSort) => {
      if (!currentSort || currentSort.index !== index) return { index, dir: 'asc' };
      return currentSort.dir === 'asc' ? { index, dir: 'desc' } : null;
    });

  const toggleRow = (key: number) =>
    setSelected((currentSelection) => {
      const next = new Set(currentSelection);
      if (!next.delete(key)) next.add(key);
      return next;
    });

  /** The TUI has `PgUp/PgDn/Home/End`; so does this. */
  const onKey = (event: React.KeyboardEvent) => {
    const go = (value: number) => {
      event.preventDefault();
      setPage(clampPage(value, ordered.length, step));
    };
    if (event.key === 'PageDown') go(current + 1);
    else if (event.key === 'PageUp') go(current - 1);
    else if (event.key === 'Home') go(0);
    else if (event.key === 'End') go(pages - 1);
  };

  const focusedValue =
    cell === null ? null : (ordered.find((r) => r.key === cell.row)?.cells[cell.col] ?? '');

  return (
    <div className="flex max-h-full min-h-0 flex-col overflow-hidden border border-code-edge bg-code">
      <div className="flex items-center gap-2 border-b border-code-edge bg-panel px-2 py-1">
        <span className="min-w-0 truncate text-ui font-bold text-white">
          {artifact.name || 'table'}
        </span>
        <span className="min-w-0 flex-1 truncate text-chip text-muted">
          {`${ordered.length} × ${header.length} · ${artifact.sizeLabel}`}
        </span>
        <button
          type="button"
          className="flex min-h-11 shrink-0 items-center border border-edge-strong px-3 text-ui text-code-foreground active:bg-hover"
        >
          {selected.size > 0 ? `Copy ${selected.size} rows` : 'Copy CSV'}
        </button>
      </div>

      <div className="relative flex min-h-0 flex-1">
        <div
          ref={fitRef}
          tabIndex={0}
          onKeyDown={onKey}
          aria-label={artifact.name || 'table'}
          className="min-h-0 min-w-0 flex-1 overflow-auto overscroll-contain"
        >
          <table className="w-auto border-collapse text-meta text-code-foreground">
            <thead>
              <tr>
                <th
                  scope="col"
                  className={`${HEAD_CELL} left-0 z-20 border-r border-code-edge px-2 text-right text-chip font-bold text-white`}
                >
                  #
                </th>
                {header.map((label, index) => (
                  <th
                    key={index}
                    scope="col"
                    aria-sort={
                      sort?.index === index
                        ? sort.dir === 'asc'
                          ? 'ascending'
                          : 'descending'
                        : 'none'
                    }
                    className={`${HEAD_CELL} ${index > 0 ? 'border-l border-code-edge' : ''}`}
                  >
                    <button
                      type="button"
                      onClick={() => toggleSort(index)}
                      className={`flex h-full w-full max-w-[34ch] items-center gap-1 px-2 text-chip font-bold tracking-wide text-white uppercase ${
                        aligns[index] ? 'justify-end' : 'justify-start'
                      }`}
                    >
                      <span className="truncate">{label || ' '}</span>
                      <span
                        className={
                          sort?.index === index ? 'shrink-0 text-accent-ink' : 'shrink-0 text-muted'
                        }
                      >
                        {sort?.index === index ? (sort.dir === 'asc' ? '▲' : '▼') : '⇅'}
                      </span>
                    </button>
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {shown.map((row, offset) => {
                const isSelected = selected.has(row.key);
                return (
                  <tr
                    key={row.key}
                    className={isSelected ? 'bg-hover' : 'even:bg-panel-2 hover:bg-hover'}
                  >
                    <td
                      className={`${BODY_CELL} sticky left-0 z-10 border-r border-code-edge ${
                        isSelected ? 'bg-hover' : 'bg-panel-2'
                      }`}
                    >
                      <button
                        type="button"
                        aria-pressed={isSelected}
                        aria-label={`Select row ${range.first + offset}`}
                        onClick={() => toggleRow(row.key)}
                        className={`h-full w-full px-2 text-right text-chip tabular-nums ${
                          isSelected ? 'text-accent-ink' : 'text-muted'
                        }`}
                      >
                        {isSelected ? '✓' : range.first + offset}
                      </button>
                    </td>
                    {header.map((_, index) => {
                      const value = row.cells[index] ?? '';
                      const focused = cell?.row === row.key && cell.col === index;
                      return (
                        <td
                          key={index}
                          className={`${BODY_CELL} ${index > 0 ? 'border-l border-code-edge' : ''}`}
                        >
                          <button
                            type="button"
                            onClick={() => setCell({ row: row.key, col: index })}
                            className={`block h-full w-full max-w-[34ch] truncate px-2 ${
                              aligns[index] ? 'text-right tabular-nums' : 'text-left'
                            } ${focused ? 'bg-code-ok ring-1 ring-accent ring-inset' : ''}`}
                          >
                            {value === '' ? <span className="text-muted italic">NULL</span> : value}
                          </button>
                        </td>
                      );
                    })}
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
        {fit.overflowX && (
          <div
            aria-hidden
            className="pointer-events-none absolute inset-y-0 right-0 w-10 border-r-2 border-accent-ink bg-gradient-to-l from-code via-code to-transparent"
          />
        )}
      </div>

      {cell !== null && (
        <div className="flex items-start gap-2 border-t border-code-edge bg-panel px-2 py-1.5">
          <span className="shrink-0 pt-0.5 text-chip text-muted">
            {`${header[cell.col] ?? ''} · row ${cell.row + 1}`}
          </span>
          <pre className="max-h-20 min-w-0 flex-1 overflow-auto text-meta break-all whitespace-pre-wrap text-code-foreground">
            {focusedValue === '' ? 'NULL' : focusedValue}
          </pre>
          <button
            type="button"
            className="flex min-h-11 shrink-0 items-center border border-edge-strong px-3 text-ui text-code-foreground active:bg-hover"
          >
            Copy value
          </button>
        </div>
      )}

      {paged && (
        <div className="flex flex-wrap items-center gap-2 border-t border-code-edge bg-panel px-2 py-1">
          <label className="flex shrink-0 items-center gap-1 text-ui text-code-foreground">
            Rows
            <select
              value={size}
              aria-label="Rows per page"
              onChange={(event) => {
                const next = event.target.value;
                setSize(next === 'fit' ? 'fit' : Number(next));
                setPage(0);
              }}
              className="min-h-11 bg-input px-2 text-ui text-code-foreground"
            >
              <option value="fit">Fit</option>
              {PAGE_SIZES.map((value) => (
                <option key={value} value={value}>
                  {value}
                </option>
              ))}
            </select>
          </label>
          <span className="min-w-0 flex-1 truncate text-ui text-code-foreground" aria-live="polite">
            {`${range.first}–${range.last} of ${ordered.length}`}
            {selected.size > 0 ? ` · ${selected.size} selected` : ''}
          </span>
          <span className="shrink-0 text-chip text-muted">PgUp/PgDn</span>
          <button
            type="button"
            disabled={current === 0}
            aria-label="Previous page"
            onClick={() => setPage(current - 1)}
            className={PAGER_BUTTON}
          >
            Prev
          </button>
          <span className="shrink-0 text-ui tabular-nums text-code-foreground">{`${current + 1}/${pages}`}</span>
          <button
            type="button"
            disabled={current >= pages - 1}
            aria-label="Next page"
            onClick={() => setPage(current + 1)}
            className={PAGER_BUTTON}
          >
            Next
          </button>
        </div>
      )}
    </div>
  );
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
  tall: fence('events.csv', 'gateway journal', TALL_CSV),
  wide: fence('ci-runs.csv', 'last five CI runs', WIDE_CSV),
  solo: fence('count.csv', '', SOLO_CSV),
  blob: fence('tool-calls.csv', 'raw tool payloads', BLOB_CSV),
};

function Frame({ caption, children }: { caption: string; children: React.ReactNode }) {
  return (
    <section
      aria-label="Attached table"
      className="mx-auto flex h-full min-h-0 w-full max-w-[1400px] flex-col gap-2 p-3 sm:p-6"
    >
      <p className="shrink-0 font-mono text-meta text-dialog-hint">{caption}</p>
      {children}
    </section>
  );
}

/** Proposal 1: content-width grid, `#` gutter, real head, `Fit` paging. */
export function RuledGridVariant({ state }: { state: string }) {
  if (state === 'sorted') {
    return (
      <Frame caption="vis_attach — sorted by sessions, third click on the head clears it">
        <ProposedGrid body={SHEETS.default} sort={{ index: 2, dir: 'desc' }} />
      </Frame>
    );
  }
  return (
    <Frame caption={`vis_attach — ruled grid, ${state}`}>
      <ProposedGrid body={SHEETS[state] ?? SHEETS.default} />
    </Frame>
  );
}

/** Proposal 2: the same grid answering "what is IN that cell / those rows". */
export function InspectGridVariant({ state }: { state: string }) {
  if (state === 'rows') {
    return (
      <Frame caption="vis_attach — gutter multi-select, inspector closed">
        <ProposedGrid body={SHEETS.default} preselect={[0, 2, 4]} />
      </Frame>
    );
  }
  if (state === 'blob') {
    return (
      <Frame caption="vis_attach — one 4 KB cell, truncated into the inspector">
        <ProposedGrid body={SHEETS.blob} inspect={{ row: 0, col: 2 }} />
      </Frame>
    );
  }
  return (
    <Frame caption="vis_attach — cell focus, not row toggle">
      <ProposedGrid body={SHEETS.default} inspect={{ row: 4, col: 1 }} />
    </Frame>
  );
}
