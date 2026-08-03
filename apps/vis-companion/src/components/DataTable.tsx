import { memo, useMemo, useState } from 'react';

// A CSV/TSV artifact is DATA, not a picture. `vis_attach` emits it as a
// ````vis-table` fence and BOTH surfaces paint it as a real grid: the TUI through
// `channel_tui/table.clj`, the companion through this component. Parsing,
// filtering and sorting are pure functions so the behaviour is testable without a
// DOM, exactly like the TUI's primitives.

export type TableArtifact = {
  /** `[Table: fleet.csv 3 rows × 3 cols, 64 B]` — the caption row. */
  summary: string;
  name: string;
  mime: string;
  cols: number | null;
  rows: number | null;
  sizeLabel: string;
  csv: string;
};

/** Parse a `vis-table` fence body: five header lines, then the CSV payload. */
export function parseTableBlock(body: string): TableArtifact {
  const lines = String(body ?? '').split('\n');
  const [summary = '', name = '', mime = '', dims = '', sizeLabel = ''] = lines;
  const [rawCols, rawRows] = dims.split('x');
  const num = (value: string | undefined) => {
    const parsed = Number.parseInt((value ?? '').trim(), 10);
    return Number.isFinite(parsed) ? parsed : null;
  };
  return {
    summary: summary.trim(),
    name: name.trim(),
    mime: mime.trim(),
    cols: num(rawCols),
    rows: num(rawRows),
    sizeLabel: sizeLabel.trim(),
    csv: lines.slice(5).join('\n'),
  };
}

/**
 * RFC-4180 parse into row vectors: quoted fields, doubled `""` escapes and
 * embedded newlines included. Every row is padded to the widest one, so
 * `row[i]` is total across the grid.
 */
export function parseCsv(text: string): string[][] {
  const source = String(text ?? '').replace(/\r\n/g, '\n');
  const rows: string[][] = [];
  let row: string[] = [];
  let field = '';
  let quoted = false;
  let started = false;
  for (let i = 0; i < source.length; i += 1) {
    const c = source[i];
    if (quoted) {
      if (c === '"' && source[i + 1] === '"') {
        field += '"';
        i += 1;
      } else if (c === '"') {
        quoted = false;
      } else {
        field += c;
      }
      continue;
    }
    if (c === '"') {
      quoted = true;
      started = true;
    } else if (c === ',') {
      row.push(field);
      field = '';
      started = true;
    } else if (c === '\n') {
      row.push(field);
      rows.push(row);
      row = [];
      field = '';
      started = false;
    } else {
      field += c;
      started = true;
    }
  }
  if (started || field.length > 0 || row.length > 0) {
    row.push(field);
    rows.push(row);
  }
  const width = rows.reduce((max, r) => Math.max(max, r.length), 0);
  return rows.map((r) => (r.length === width ? r : [...r, ...Array(width - r.length).fill('')]));
}

/**
 * A cell as a number for sorting/alignment. Thousands separators, a leading
 * currency mark and a trailing `%` are formatting, not text — a `1,234.5` column
 * still sorts numerically. `null` when the cell is not a number.
 */
export function csvNumber(cell: string): number | null {
  const cleaned = String(cell ?? '')
    .trim()
    .replace(/[,_\s%$€£]/g, '');
  if (cleaned === '') return null;
  const value = Number(cleaned);
  return Number.isFinite(value) ? value : null;
}

/** True when every non-blank cell of the DATA rows under `index` reads as a number. */
export function isNumericColumn(rows: string[][], index: number): boolean {
  const values = rows.map((r) => String(r[index] ?? '').trim()).filter((v) => v !== '');
  return values.length > 0 && values.every((v) => csvNumber(v) !== null);
}

export type TableRow = { key: number; cells: string[] };

/** Case-insensitive substring match against ANY cell. A blank query matches everything. */
export function rowMatches(cells: string[], query: string): boolean {
  const q = String(query ?? '')
    .trim()
    .toLowerCase();
  if (q === '') return true;
  return cells.some((cell) => String(cell ?? '').toLowerCase().includes(q));
}

export function filterRows(rows: TableRow[], query: string): TableRow[] {
  return rows.filter((row) => rowMatches(row.cells, query));
}

/**
 * Sort by column `index`. A column whose every non-blank cell parses as a number
 * sorts NUMERICALLY (so 9 comes before 10), any other column case-insensitively;
 * blanks sort last. Stable: rows keep their source order inside a tie.
 */
export function sortRows(rows: TableRow[], index: number, dir: 'asc' | 'desc'): TableRow[] {
  const numeric = isNumericColumn(
    rows.map((r) => r.cells),
    index,
  );
  const sign = dir === 'desc' ? -1 : 1;
  return [...rows].sort((a, b) => {
    const left = String(a.cells[index] ?? '').trim();
    const right = String(b.cells[index] ?? '').trim();
    if (left === '' || right === '') {
      if (left === right) return a.key - b.key;
      return left === '' ? 1 : -1;
    }
    if (numeric) {
      const diff = (csvNumber(left) ?? 0) - (csvNumber(right) ?? 0);
      return diff === 0 ? a.key - b.key : sign * diff;
    }
    const cmp = left.toLowerCase().localeCompare(right.toLowerCase());
    return cmp === 0 ? a.key - b.key : sign * cmp;
  });
}

/** Render rows back to CSV — what Copy hands to the clipboard. */
export function toCsv(rows: string[][]): string {
  return rows
    .map((row) =>
      row
        .map((cell) => {
          const value = String(cell ?? '');
          return /[",\n]/.test(value) ? `"${value.replace(/"/g, '""')}"` : value;
        })
        .join(','),
    )
    .join('\n');
}

const HEAD_CELL =
  'sticky top-0 z-10 bg-panel-2 px-2 py-1.5 text-left align-bottom font-semibold text-code-foreground';

export const DataTable = memo(function DataTable({
  body,
  compact,
  frameless = false,
}: {
  body: string;
  compact: boolean;
  /** Keep the spacing but drop the frame: an enclosing card already draws one. */
  frameless?: boolean;
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

  const [query, setQuery] = useState('');
  const [sort, setSort] = useState<{ index: number; dir: 'asc' | 'desc' } | null>(null);
  const [selected, setSelected] = useState<ReadonlySet<number>>(new Set());
  const [copied, setCopied] = useState(false);

  const visible = useMemo(() => {
    const matched = filterRows(rows, query);
    return sort ? sortRows(matched, sort.index, sort.dir) : matched;
  }, [rows, query, sort]);

  const chosen = visible.filter((row) => selected.has(row.key));
  const label = artifact.name || 'table';

  const toggleSort = (index: number) =>
    setSort((current) =>
      current && current.index === index
        ? { index, dir: current.dir === 'asc' ? 'desc' : 'asc' }
        : { index, dir: 'asc' },
    );

  const toggleRow = (key: number) =>
    setSelected((current) => {
      const next = new Set(current);
      if (!next.delete(key)) next.add(key);
      return next;
    });

  const copy = () => {
    const payload = (chosen.length > 0 ? chosen : visible).map((row) => row.cells);
    void navigator.clipboard?.writeText(toCsv([header, ...payload]));
    setCopied(true);
    setTimeout(() => setCopied(false), 1200);
  };

  return (
    <div
      className={`${compact ? 'my-2' : 'my-3'} overflow-hidden bg-code ${frameless ? '' : 'border border-code-edge'}`}
    >
      <div className="flex flex-wrap items-center gap-2 border-b border-code-edge px-2 py-1.5">
        <span className="min-w-0 flex-1 truncate text-chip text-muted">
          {artifact.summary || label}
        </span>
        <span className="shrink-0 text-chip text-muted" aria-live="polite">
          {visible.length === rows.length
            ? `${rows.length} ${rows.length === 1 ? 'row' : 'rows'}`
            : `${visible.length}/${rows.length} rows`}
          {selected.size > 0 ? ` · ${selected.size} selected` : ''}
        </span>
        <button
          type="button"
          onClick={copy}
          className="shrink-0 border border-edge px-2 py-0.5 text-chip text-muted active:bg-hover"
        >
          {copied ? 'Copied' : chosen.length > 0 ? `Copy ${chosen.length}` : 'Copy CSV'}
        </button>
      </div>
      <div className="px-2 py-1.5">
        <input
          type="search"
          value={query}
          onChange={(event) => setQuery(event.target.value)}
          placeholder="Filter rows…"
          aria-label={`Filter rows of ${label}`}
          className="w-full bg-input px-2 py-1 text-meta text-code-foreground placeholder:text-muted"
        />
      </div>
      <div className="max-h-[60vh] max-w-full overflow-auto overscroll-x-contain">
        <table className="w-full border-collapse text-meta text-code-foreground" aria-label={label}>
          <thead>
            <tr>
              {header.map((cell, index) => (
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
                  className={`${HEAD_CELL} ${aligns[index] ? 'text-right' : 'text-left'}`}
                >
                  <button
                    type="button"
                    onClick={() => toggleSort(index)}
                    className="max-w-full truncate whitespace-nowrap active:text-accent"
                  >
                    {cell || ' '}
                    {sort?.index === index ? (sort.dir === 'asc' ? ' ▲' : ' ▼') : ''}
                  </button>
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {visible.map((row) => (
              <tr
                key={row.key}
                aria-selected={selected.has(row.key)}
                tabIndex={0}
                onClick={() => toggleRow(row.key)}
                onKeyDown={(event) => {
                  if (event.key === 'Enter' || event.key === ' ') {
                    event.preventDefault();
                    toggleRow(row.key);
                  }
                }}
                className={`border-t border-code-edge ${selected.has(row.key) ? 'bg-hover' : ''}`}
              >
                {header.map((_, index) => (
                  <td
                    key={index}
                    className={`px-2 py-1 whitespace-nowrap ${aligns[index] ? 'text-right tabular-nums' : 'text-left'}`}
                  >
                    {row.cells[index] ?? ''}
                  </td>
                ))}
              </tr>
            ))}
            {visible.length === 0 && (
              <tr>
                <td
                  colSpan={Math.max(1, header.length)}
                  className="px-2 py-2 text-center text-meta text-muted"
                >
                  No row matches this filter
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
    </div>
  );
});
