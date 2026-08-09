import { memo, useCallback, useId, useMemo, useRef, useState } from 'react';
import { SortIcon } from './icons';
import { Button } from './ui';

// A CSV/TSV artifact is DATA, not a picture. `attach` emits it as a
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

/** One data row, in px: the unit `Fit` divides the free height by. */
const ROW_H = 26;
/** The sticky head, in px. */
const HEAD_H = 30;
/** The scroller's cap, as a fraction of the viewport — what `Fit` fills. */
const VIEW_FRACTION = 0.6;

/**
 * The sheet's own palette, and it is deliberately NOT another grey code block.
 * Its ground is the input surface (paper / ink), its head is an amber band under
 * a 2 px rule — a header you can still find with the page scrolled — selection
 * is BLUE, the focused cell is AMBER, and numbers are typed in the code number
 * hue. Three roles, three colours, instead of the 8/255 tint that shipped first.
 */
const SHEET = 'bg-input';
const HEAD_CELL =
  'sticky top-0 z-10 h-[30px] border-b-2 border-warn-strong bg-warn-surface p-0 align-middle';
const BODY_CELL = 'h-[26px] border-b border-code-edge p-0 align-middle';

/** The `│` of the TUI grid: every column but the first carries its own rule. */
const COLUMN_RULE = 'border-l border-code-edge';

type Sort = { index: number; dir: 'asc' | 'desc' } | null;
type Cell = { row: number; col: number } | null;

/**
 * Rows that FILL the box. Measured off the viewport, never off the grid's own
 * content: a page sized from the scroller's current height feeds its own height
 * back into the measurement and oscillates.
 */
function fitRows(): number {
  if (typeof window === 'undefined') return PAGE_SIZES[1];
  return Math.max(4, Math.floor((window.innerHeight * VIEW_FRACTION - HEAD_H) / ROW_H));
}

/** Sheet metrics: the `Fit` page size, and whether it really scrolls sideways. */
function useSheet(): [{ rows: number; overflowX: boolean }, (node: HTMLDivElement | null) => void] {
  const [sheet, setSheet] = useState({ rows: fitRows(), overflowX: false });
  const observer = useRef<ResizeObserver | null>(null);
  const ref = useCallback((node: HTMLDivElement | null) => {
    observer.current?.disconnect();
    observer.current = null;
    if (!node) return;
    const measure = () =>
      setSheet({ rows: fitRows(), overflowX: node.scrollWidth > node.clientWidth + 1 });
    const ro = new ResizeObserver(measure);
    ro.observe(node);
    observer.current = ro;
    measure();
  }, []);
  return [sheet, ref];
}

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

  const gridId = useId();
  const [sort, setSort] = useState<Sort>(null);
  const [selected, setSelected] = useState<ReadonlySet<number>>(new Set());
  const [cell, setCell] = useState<Cell>(null);
  const [copied, setCopied] = useState(false);
  const [pageSize, setPageSize] = useState<number | 'fit'>(PAGE_SIZES[1]);
  const [page, setPage] = useState(0);
  const [sheet, sheetRef] = useSheet();

  const ordered = useMemo(
    () => (sort ? sortRows(rows, sort.index, sort.dir) : rows),
    [rows, sort],
  );

  const step = pageSize === 'fit' ? sheet.rows : pageSize;
  const pages = pageCount(ordered.length, step);
  const current = clampPage(page, ordered.length, step);
  const shown = useMemo(() => pageRows(ordered, current, step), [ordered, current, step]);
  const range = pageRange(ordered.length, current, step);
  /** A sheet nobody would ever page through must not grow a pager. */
  const paged = ordered.length > PAGE_SIZES[0];

  const chosen = ordered.filter((row) => selected.has(row.key));
  const label = artifact.name || 'table';
  const cellId = (row: number, col: number) => `${gridId}-c${row}-${col}`;

  /** asc → desc → OFF: the file's own order stays reachable on the third click. */
  const toggleSort = (index: number) => {
    setSort((currentSort) => {
      if (!currentSort || currentSort.index !== index) return { index, dir: 'asc' };
      return currentSort.dir === 'asc' ? { index, dir: 'desc' } : null;
    });
    setPage(0);
  };

  const toggleRow = (key: number) =>
    setSelected((currentSelection) => {
      const next = new Set(currentSelection);
      if (!next.delete(key)) next.add(key);
      return next;
    });

  /** The gutter head is the page's select-all, and the way back out of a selection. */
  const toggleAll = () =>
    setSelected((currentSelection) =>
      currentSelection.size > 0 ? new Set() : new Set(shown.map((row) => row.key)),
    );

  const goPage = (value: number) => setPage(clampPage(value, ordered.length, step));

  /** Walk the focused cell; a step past the page edge pulls the page with it. */
  const moveCell = (rowStep: number, colStep: number) => {
    if (header.length === 0 || shown.length === 0) return;
    if (cell === null) {
      setCell({ row: shown[0].key, col: 0 });
      return;
    }
    const col = Math.min(header.length - 1, Math.max(0, cell.col + colStep));
    if (rowStep === 0) {
      setCell({ row: cell.row, col });
      return;
    }
    const at = Math.max(
      0,
      shown.findIndex((row) => row.key === cell.row),
    );
    const next = at + rowStep;
    if (next < 0 || next >= shown.length) {
      const target = clampPage(current + Math.sign(rowStep), ordered.length, step);
      if (target === current) return;
      const landing = pageRows(ordered, target, step);
      setPage(target);
      setCell({ row: (next < 0 ? landing[landing.length - 1] : landing[0]).key, col });
      return;
    }
    setCell({ row: shown[next].key, col });
  };

  /** The TUI has `↑/↓`, `←/→`, `PgUp/PgDn`, `Home/End`; so does this. */
  const onKey = (event: React.KeyboardEvent) => {
    const handled: Record<string, () => void> = {
      PageDown: () => goPage(current + 1),
      PageUp: () => goPage(current - 1),
      Home: () => goPage(0),
      End: () => goPage(pages - 1),
      ArrowDown: () => moveCell(1, 0),
      ArrowUp: () => moveCell(-1, 0),
      ArrowRight: () => moveCell(0, 1),
      ArrowLeft: () => moveCell(0, -1),
      ' ': () => cell && toggleRow(cell.row),
      Enter: () => cell && toggleRow(cell.row),
      Escape: () => setCell(null),
    };
    const act = handled[event.key];
    if (!act) return;
    event.preventDefault();
    act();
  };

  const copy = () => {
    const payload = (chosen.length > 0 ? chosen : ordered).map((row) => row.cells);
    void navigator.clipboard?.writeText(toCsv([header, ...payload]));
    setCopied(true);
    setTimeout(() => setCopied(false), 1200);
  };

  const focused =
    cell === null ? null : (ordered.find((row) => row.key === cell.row)?.cells[cell.col] ?? '');

  return (
    <div
      className={`${compact ? 'my-2' : 'my-3'} flex max-h-full w-fit max-w-full min-h-0 flex-col overflow-hidden ${SHEET} ${frameless ? '' : 'border border-code-edge'}`}
    >
      <div className="flex flex-wrap items-center gap-2 border-b border-code-edge bg-panel px-2 py-1">
        <span className="min-w-0 flex-1 truncate text-chip text-muted">
          {artifact.summary || label}
        </span>
        <span className="shrink-0 text-chip text-warn" aria-live="polite">
          {selected.size > 0 ? `${selected.size} selected` : ''}
        </span>
        <Button variant="secondary" density="compact" onClick={copy}>
          {copied ? 'Copied' : chosen.length > 0 ? `Copy ${chosen.length} rows` : 'Copy CSV'}
        </Button>
      </div>

      <div className="relative flex min-h-0 flex-1">
        <div
          ref={sheetRef}
          className="max-h-[60vh] min-h-0 min-w-0 flex-1 overflow-auto overscroll-x-contain"
        >
          <table
            role="grid"
            tabIndex={0}
            onKeyDown={onKey}
            aria-label={label}
            aria-rowcount={ordered.length + 1}
            aria-colcount={header.length + 1}
            aria-activedescendant={cell === null ? undefined : cellId(cell.row, cell.col)}
            className="w-auto border-collapse text-meta text-code-foreground focus-visible:outline-2 focus-visible:outline-warn-strong"
          >
            <thead>
              <tr>
                <th
                  scope="col"
                  className={`${HEAD_CELL} left-0 z-20 border-r border-code-edge`}
                >
                  <button
                    type="button"
                    tabIndex={-1}
                    onClick={toggleAll}
                    aria-label={selected.size > 0 ? 'Clear selection' : 'Select every row on this page'}
                    className="h-full w-full px-2 text-right text-chip font-bold text-warn"
                  >
                    #
                  </button>
                </th>
                {header.map((cellLabel, index) => (
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
                      className={`flex h-full w-full max-w-[34ch] items-center gap-1 px-2 text-chip font-bold tracking-wide text-warn uppercase ${
                        aligns[index] ? 'justify-end' : 'justify-start'
                      }`}
                    >
                      <span className="truncate">{cellLabel || ' '}</span>
                      <SortIcon
                        dir={sort?.index === index ? sort.dir : undefined}
                        className={sort?.index === index ? 'size-3' : 'size-3 opacity-50'}
                      />
                    </button>
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {shown.map((row, offset) => {
                const picked = selected.has(row.key);
                return (
                  <tr
                    key={row.key}
                    aria-selected={picked}
                    aria-rowindex={range.first + offset + 1}
                    className={
                      picked
                        ? 'bg-result-path text-result-path-foreground'
                        : 'even:bg-panel-2 hover:bg-hover'
                    }
                  >
                    <td
                      className={`${BODY_CELL} sticky left-0 z-10 border-r border-code-edge ${picked ? 'bg-result-path' : 'bg-panel-2'}`}
                    >
                      <button
                        type="button"
                        tabIndex={-1}
                        aria-pressed={picked}
                        aria-label={`Select row ${range.first + offset}`}
                        onClick={() => toggleRow(row.key)}
                        className={`h-full w-full px-2 text-right text-chip tabular-nums ${picked ? 'text-result-path-foreground' : 'text-muted'}`}
                      >
                        {picked ? '✓' : range.first + offset}
                      </button>
                    </td>
                    {header.map((_, index) => {
                      const value = row.cells[index] ?? '';
                      const here = cell?.row === row.key && cell.col === index;
                      return (
                        <td
                          key={index}
                          id={cellId(row.key, index)}
                          onClick={() => setCell({ row: row.key, col: index })}
                          className={`${BODY_CELL} ${index > 0 ? COLUMN_RULE : ''} ${
                            here ? 'bg-warn-surface ring-2 ring-warn-strong ring-inset' : ''
                          }`}
                        >
                          <span
                            className={`block max-w-[34ch] truncate px-2 whitespace-nowrap ${
                              aligns[index]
                                ? `text-right tabular-nums ${picked ? '' : 'text-code-syntax-number'}`
                                : 'text-left'
                            }`}
                          >
                            {value === '' ? <span className="text-muted italic">NULL</span> : value}
                          </span>
                        </td>
                      );
                    })}
                  </tr>
                );
              })}
              {ordered.length === 0 && (
                <tr>
                  <td
                    colSpan={Math.max(1, header.length) + 1}
                    className="px-2 py-2 text-center text-meta text-muted"
                  >
                    No rows
                  </td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
        {sheet.overflowX && (
          <div
            aria-hidden
            className="pointer-events-none absolute inset-y-0 right-0 w-6 border-r-2 border-warn-strong bg-gradient-to-l from-input to-transparent"
          />
        )}
      </div>

      {cell !== null && (
        <div className="flex items-start gap-2 border-t-2 border-warn-strong bg-panel px-2 py-1.5">
          <span className="shrink-0 pt-0.5 text-chip text-warn">
            {`${header[cell.col] ?? ''} · row ${cell.row + 1}`}
          </span>
          <pre className="max-h-20 min-w-0 flex-1 overflow-auto text-meta break-all whitespace-pre-wrap text-code-foreground">
            {focused === '' ? 'NULL' : focused}
          </pre>
          <Button
            variant="secondary"
            density="compact"
            onClick={() => void navigator.clipboard?.writeText(focused ?? '')}
          >
            Copy value
          </Button>
        </div>
      )}

      {paged && (
        <div className="flex flex-wrap items-center gap-2 border-t border-code-edge bg-panel px-2 py-1">
          <label className="flex shrink-0 items-center gap-1 text-ui text-code-foreground">
            Rows
            <select
              value={pageSize}
              aria-label={`Rows per page of ${label}`}
              onChange={(event) => {
                const next = event.target.value;
                setPageSize(next === 'fit' ? 'fit' : Number(next));
                setPage(0);
              }}
              className="min-h-11 bg-input px-2 text-ui text-code-foreground"
            >
              <option value="fit">Fit</option>
              {PAGE_SIZES.map((size) => (
                <option key={size} value={size}>
                  {size}
                </option>
              ))}
            </select>
          </label>
          <span className="min-w-0 flex-1 truncate text-ui text-code-foreground">
            {`${range.first}–${range.last} of ${ordered.length}`}
          </span>
          <span className="hidden shrink-0 text-chip text-muted sm:inline">PgUp/PgDn</span>
          <Button
            variant="secondary"
            density="compact"
            onClick={() => goPage(current - 1)}
            disabled={current === 0}
            aria-label="Previous page"
          >
            Prev
          </Button>
          <span className="shrink-0 text-ui tabular-nums text-code-foreground">{`Page ${current + 1}/${pages}`}</span>
          <Button
            variant="secondary"
            density="compact"
            onClick={() => goPage(current + 1)}
            disabled={current >= pages - 1}
            aria-label="Next page"
          >
            Next
          </Button>
        </div>
      )}
    </div>
  );
});
