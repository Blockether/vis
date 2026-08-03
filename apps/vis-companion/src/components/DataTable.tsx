import { memo, useMemo, useState } from 'react';

// A CSV/TSV artifact is DATA, not a picture. `vis_attach` emits it as a
// ````vis-table` fence and BOTH surfaces paint it as a real grid: the TUI through
// `channel_tui/table.clj`, the companion through this component. Parsing, paging
// and sorting are pure functions so the behaviour is testable without a DOM,
// exactly like the TUI's primitives.

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

/** Page sizes the pager offers; the first one is also the threshold that shows it. */
export const PAGE_SIZES = [10, 25, 50, 100] as const;

/** How many pages `total` rows fill — never less than one, so page 1/1 always exists. */
export function pageCount(total: number, size: number): number {
  return Math.max(1, Math.ceil(Math.max(0, total) / Math.max(1, size)));
}

/** Clamp a page index into the pages `total` rows actually have. */
export function clampPage(page: number, total: number, size: number): number {
  return Math.min(Math.max(0, Math.trunc(page)), pageCount(total, size) - 1);
}

/** The rows of one page — the ONLY rows the grid paints. */
export function pageRows<T>(rows: readonly T[], page: number, size: number): T[] {
  const step = Math.max(1, size);
  const start = clampPage(page, rows.length, step) * step;
  return rows.slice(start, start + step);
}

/** `1–25 of 60`, or `0 of 0` for an empty sheet. 1-based, inclusive. */
export function pageRange(
  total: number,
  page: number,
  size: number,
): { first: number; last: number } {
  if (total <= 0) return { first: 0, last: 0 };
  const step = Math.max(1, size);
  const start = clampPage(page, total, step) * step;
  return { first: start + 1, last: Math.min(total, start + step) };
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

/** The `│` of the TUI grid: every column but the first carries its own rule. */
const COLUMN_RULE = 'border-l border-code-edge';

const PAGER_BUTTON = 'border border-edge px-2 py-0.5 text-chip text-muted active:bg-hover';

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

  const [sort, setSort] = useState<{ index: number; dir: 'asc' | 'desc' } | null>(null);
  const [selected, setSelected] = useState<ReadonlySet<number>>(new Set());
  const [copied, setCopied] = useState(false);
  const [pageSize, setPageSize] = useState<number>(25);
  const [page, setPage] = useState(0);

  const ordered = useMemo(
    () => (sort ? sortRows(rows, sort.index, sort.dir) : rows),
    [rows, sort],
  );

  const pages = pageCount(ordered.length, pageSize);
  const current = clampPage(page, ordered.length, pageSize);
  const shown = useMemo(() => pageRows(ordered, current, pageSize), [ordered, current, pageSize]);
  const range = pageRange(ordered.length, current, pageSize);
  /** A sheet nobody would ever page through must not grow a pager. */
  const paged = ordered.length > PAGE_SIZES[0];

  const chosen = ordered.filter((row) => selected.has(row.key));
  const label = artifact.name || 'table';

  const toggleSort = (index: number) => {
    setSort((currentSort) =>
      currentSort && currentSort.index === index
        ? { index, dir: currentSort.dir === 'asc' ? 'desc' : 'asc' }
        : { index, dir: 'asc' },
    );
    setPage(0);
  };

  const toggleRow = (key: number) =>
    setSelected((currentSelection) => {
      const next = new Set(currentSelection);
      if (!next.delete(key)) next.add(key);
      return next;
    });

  const copy = () => {
    const payload = (chosen.length > 0 ? chosen : ordered).map((row) => row.cells);
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
          {`${rows.length} ${rows.length === 1 ? 'row' : 'rows'}`}
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
                  className={`${HEAD_CELL} ${index > 0 ? COLUMN_RULE : ''} ${aligns[index] ? 'text-right' : 'text-left'}`}
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
            {shown.map((row) => (
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
                    className={`px-2 py-1 whitespace-nowrap ${index > 0 ? COLUMN_RULE : ''} ${aligns[index] ? 'text-right tabular-nums' : 'text-left'}`}
                  >
                    {row.cells[index] ?? ''}
                  </td>
                ))}
              </tr>
            ))}
            {ordered.length === 0 && (
              <tr>
                <td
                  colSpan={Math.max(1, header.length)}
                  className="px-2 py-2 text-center text-meta text-muted"
                >
                  No rows
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      {paged && (
        <div className="flex flex-wrap items-center gap-2 border-t border-code-edge px-2 py-1.5">
          <label className="flex shrink-0 items-center gap-1 text-chip text-muted">
            Rows
            <select
              value={pageSize}
              aria-label={`Rows per page of ${label}`}
              onChange={(event) => {
                setPageSize(Number(event.target.value));
                setPage(0);
              }}
              className="bg-input px-1 py-0.5 text-chip text-code-foreground"
            >
              {PAGE_SIZES.map((size) => (
                <option key={size} value={size}>
                  {size}
                </option>
              ))}
            </select>
          </label>
          <span className="min-w-0 flex-1 truncate text-chip text-muted" aria-live="polite">
            {`${range.first}–${range.last} of ${ordered.length}`}
          </span>
          <span className="shrink-0 text-chip text-muted">{`Page ${current + 1}/${pages}`}</span>
          <button
            type="button"
            onClick={() => setPage(Math.max(0, current - 1))}
            disabled={current === 0}
            aria-label="Previous page"
            className={`${PAGER_BUTTON} ${current === 0 ? 'opacity-40' : ''}`}
          >
            Prev
          </button>
          <button
            type="button"
            onClick={() => setPage(Math.min(pages - 1, current + 1))}
            disabled={current >= pages - 1}
            aria-label="Next page"
            className={`${PAGER_BUTTON} ${current >= pages - 1 ? 'opacity-40' : ''}`}
          >
            Next
          </button>
        </div>
      )}
    </div>
  );
});
