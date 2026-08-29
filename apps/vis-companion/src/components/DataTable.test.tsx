// @vitest-environment jsdom
import { cleanup, render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { renderToStaticMarkup } from 'react-dom/server';
import { afterEach, describe, expect, it } from 'vitest';
import {
  DataTable,
  clampPage,
  csvNumber,
  isNumericColumn,
  pageCount,
  pageRange,
  pageRows,
  parseCsv,
  parseTableBlock,
  sortRows,
  toCsv,
  type TableRow,
} from './DataTable';

/** The block `attach` emits for a CSV artifact: five header lines, then the payload. */
const fence = [
  '[Table: fleet.csv 3 rows × 3 cols, 64 B]',
  'fleet.csv',
  'text/csv',
  '3x3',
  '64 B',
  'name,qty,note',
  'ada,9,first',
  'yak,10,second',
  'zed,120,third',
].join('\n');

afterEach(cleanup);

const rows = (cells: string[][]): TableRow[] => cells.map((c, key) => ({ key, cells: c }));

/** Visible text of a rendered chunk: tags out, entities back. */
const text = (html: string) =>
  html
    .replace(/<[^>]+>/g, ' ')
    .replace(/&quot;/g, '"')
    .replace(/&#x27;/g, "'")
    .replace(/&gt;/g, '>')
    .replace(/&lt;/g, '<')
    .replace(/&amp;/g, '&');

describe('vis-table fence', () => {
  it('splits the five header lines from the CSV payload', () => {
    const artifact = parseTableBlock(fence);
    expect(artifact.summary).toBe('[Table: fleet.csv 3 rows × 3 cols, 64 B]');
    expect(artifact.name).toBe('fleet.csv');
    expect(artifact.mime).toBe('text/csv');
    expect(artifact.cols).toBe(3);
    expect(artifact.rows).toBe(3);
    expect(artifact.sizeLabel).toBe('64 B');
    expect(artifact.csv.split('\n')[0]).toBe('name,qty,note');
    expect(artifact.csv).toContain('zed,120,third');
  });

  it('survives a body that carries no header at all', () => {
    const artifact = parseTableBlock('');
    expect(artifact.cols).toBeNull();
    expect(artifact.csv).toBe('');
  });
});

describe('CSV parsing', () => {
  it('keeps quoted commas, doubled quotes and embedded newlines in one cell', () => {
    const grid = parseCsv('name,note\n"Doe, Jane","say ""hi""\nagain"\n');
    expect(grid).toEqual([
      ['name', 'note'],
      ['Doe, Jane', 'say "hi"\nagain'],
    ]);
  });

  it('pads ragged rows so every row has the same arity', () => {
    expect(parseCsv('a,b,c\n1\n')).toEqual([
      ['a', 'b', 'c'],
      ['1', '', ''],
    ]);
  });

  it('accepts CRLF and ignores a trailing newline', () => {
    expect(parseCsv('a,b\r\n1,2\r\n')).toEqual([
      ['a', 'b'],
      ['1', '2'],
    ]);
  });

  it('detects the semicolon columns in a UTF-8 spreadsheet export', () => {
    expect(parseCsv('\ufefflp;stanowisko;firma\r\n1;Koordynator;EOL Energia\r\n')).toEqual([
      ['lp', 'stanowisko', 'firma'],
      ['1', 'Koordynator', 'EOL Energia'],
    ]);
  });
});

describe('numbers', () => {
  it('reads formatting — separators, currency, percent — as number, not text', () => {
    expect(csvNumber('1,234.5')).toBe(1234.5);
    expect(csvNumber(' $12 ')).toBe(12);
    expect(csvNumber('45%')).toBe(45);
    expect(csvNumber('-3')).toBe(-3);
    expect(csvNumber('abc')).toBeNull();
    expect(csvNumber('')).toBeNull();
  });

  it('calls a column numeric only when every non-blank cell is one', () => {
    expect(isNumericColumn([['9'], ['10'], ['']], 0)).toBe(true);
    expect(isNumericColumn([['9'], ['n/a']], 0)).toBe(false);
    expect(isNumericColumn([[''], ['']], 0)).toBe(false);
  });
});

describe('sorting', () => {
  const data = rows([
    ['ada', '9', 'first'],
    ['yak', '10', 'second'],
    ['zed', '120', 'third'],
  ]);
  it('sorts a numeric column by magnitude, not lexicographically', () => {
    expect(sortRows(data, 1, 'asc').map((r) => r.cells[1])).toEqual(['9', '10', '120']);
    expect(sortRows(data, 1, 'desc').map((r) => r.cells[1])).toEqual(['120', '10', '9']);
  });

  it('sorts a text column case-insensitively and sinks blanks', () => {
    const mixed = rows([['Beta'], ['alpha'], ['']]);
    expect(sortRows(mixed, 0, 'asc').map((r) => r.cells[0])).toEqual(['alpha', 'Beta', '']);
    expect(sortRows(mixed, 0, 'desc').map((r) => r.cells[0])).toEqual(['Beta', 'alpha', '']);
  });

  it('round-trips selected rows back to quoted CSV', () => {
    expect(toCsv([['a', 'b'], ['x,1', 'say "hi"']])).toBe('a,b\n"x,1","say ""hi"""');
  });
});

describe('paging', () => {
  const many = rows(Array.from({ length: 60 }, (_, i) => [String(i)]));

  it('counts pages, and an empty sheet still has page 1/1', () => {
    expect(pageCount(60, 25)).toBe(3);
    expect(pageCount(50, 25)).toBe(2);
    expect(pageCount(0, 25)).toBe(1);
  });

  it('clamps a page past the end back onto the last one', () => {
    expect(clampPage(9, 60, 25)).toBe(2);
    expect(clampPage(-4, 60, 25)).toBe(0);
  });

  it('slices exactly one page, the last one short', () => {
    expect(pageRows(many, 0, 25).map((r) => r.cells[0])[0]).toBe('0');
    expect(pageRows(many, 0, 25).length).toBe(25);
    expect(pageRows(many, 1, 25).map((r) => r.cells[0])[0]).toBe('25');
    expect(pageRows(many, 2, 25).length).toBe(10);
  });

  it('reports a 1-based inclusive range', () => {
    expect(pageRange(60, 0, 25)).toEqual({ first: 1, last: 25 });
    expect(pageRange(60, 2, 25)).toEqual({ first: 51, last: 60 });
    expect(pageRange(0, 0, 25)).toEqual({ first: 0, last: 0 });
  });
});

describe('DataTable', () => {
  const html = renderToStaticMarkup(<DataTable body={fence} compact />);

  it('paints a real table with a header row and one row per record', () => {
    expect((html.match(/<tr/g) ?? []).length).toBe(4);
    // Three data columns plus the `#` gutter.
    expect((html.match(/<th\b[^>]*>/g) ?? []).length).toBe(4);
    expect(text(html)).toContain('ada');
    expect(text(html)).toContain('third');
  });

  it('offers the sort affordance and the row count, and no filter box', () => {
    expect(html).toContain('aria-sort="none"');
    expect(html).toContain('aria-label="Sort by name"');
    expect(text(html)).toContain('3 rows');
    expect(text(html)).toContain('[Table: fleet.csv 3 rows × 3 cols, 64 B]');
    expect(html).not.toContain('type="search"');
    expect(html).not.toContain('Filter rows');
  });

  it('rules every column but the first — the TUI grid\'s │, in CSS', () => {
    const heads = html.match(/<th\b[^>]*>/g) ?? [];
    // heads[0] is the `#` gutter; heads[1] is the first data column.
    expect(heads[1]).not.toContain('border-l');
  });

  it('uses the same neutral header treatment as the other document tables', () => {
    const heads = html.match(/<th\b[^>]*>/g) ?? [];
    expect(heads[0]).not.toContain('bg-warn-surface');
  });

  it('gives every row a # gutter, and the gutter IS the selection control', () => {
    expect(html).toContain('role="grid"');
    expect(html).toContain('aria-label="Select every row on this page"');
    expect(html).toContain('aria-label="Select row 1"');
    expect(html).toContain('aria-label="Select row 3"');
    expect(html).toContain('aria-selected="false"');
  });

  it('is ONE tab stop, not one per row', () => {
    expect((html.match(/tabindex="0"/g) ?? []).length).toBe(1);
  });

  it('right-aligns numeric columns without changing their ink or typeface', () => {
    expect(html).not.toContain('text-code-syntax-number');
  });

  it('shows a blank cell as NULL, not as an empty string', () => {
    const gaps = renderToStaticMarkup(
      <DataTable
        body={[
          '[Table: gaps.csv 1 rows × 2 cols, 12 B]',
          'gaps.csv',
          'text/csv',
          '2x1',
          '12 B',
          'a,b',
          '1,',
        ].join('\n')}
        compact
      />,
    );
    expect(text(gaps)).toContain('NULL');
  });

  it('says so instead of painting an empty grid', () => {
    const empty = renderToStaticMarkup(
      <DataTable body={['[Table: none.csv 0 rows × 1 cols, 0 B]', 'none.csv', 'text/csv', '1x0', '0 B', 'name'].join('\n')} compact />,
    );
    expect(text(empty)).toContain('No rows');
  });

  it('hides the pager for a sheet that fits on one page', () => {
    expect(html).not.toContain('aria-label="Next page"');
  });

  it('pages a long sheet: 25 rows, a page counter and working arrows', () => {
    const long = [
      '[Table: events.csv 60 rows × 2 cols, 1 KB]',
      'events.csv',
      'text/csv',
      '2x60',
      '1 KB',
      'n,label',
      ...Array.from({ length: 60 }, (_, i) => `${i},row-${i}`),
    ].join('\n');
    const paged = renderToStaticMarkup(<DataTable body={long} compact />);
    expect((paged.match(/<tr/g) ?? []).length).toBe(26);
    expect(text(paged)).toContain('1–25 of 60');
    expect(text(paged)).toContain('Page 1/3');
    expect(paged).toContain('aria-label="Next page"');
    expect(paged).toContain('aria-label="Rows per page of events.csv"');
    expect(text(paged)).toContain('row-24');
    expect(text(paged)).not.toContain('row-25');
  });

  it('keeps paging controls above the grid', () => {
    const long = [
      '[Table: events.csv 60 rows × 2 cols, 1 KB]',
      'events.csv',
      'text/csv',
      '2x60',
      '1 KB',
      'n,label',
      ...Array.from({ length: 60 }, (_, i) => `${i},row-${i}`),
    ].join('\n');
    const paged = renderToStaticMarkup(<DataTable body={long} compact />);
    expect(paged.indexOf('aria-label="Next page"')).toBeLessThan(paged.indexOf('role="grid"'));
  });

  // An opened artifact used to replace the spreadsheet with one vertical record on
  // touch screens. The transcript and artifact reader must expose the same real grid.
  it('keeps opened artifacts as the same table on touch screens', () => {
    const opened = renderToStaticMarkup(<DataTable body={fence} compact fill />);

    expect(opened).toContain('role="grid"');
    expect(opened).not.toContain('Record view of');
    expect(opened).not.toContain('Record 1 of');
    expect(opened).not.toContain('mouse:hidden');
  });

  // Reported from the same artifact: WebKit enlarged different table columns by different
  // amounts, while number cells and headers also introduced unrelated display colours.
  it('locks one type treatment across an opened CSV table', () => {
    const opened = renderToStaticMarkup(<DataTable body={fence} compact fill />);
    expect(opened).toContain('[-webkit-text-size-adjust:none]');
    expect(opened).toContain('[text-size-adjust:none]');
    expect(opened).not.toContain('text-code-syntax-number');
    expect(opened).toContain('touch-pan-x touch-pan-y overflow-auto');
  });

  it('closes a selected value when that cell is pressed again', async () => {
    const user = userEvent.setup();
    render(<DataTable body={fence} compact />);
    const cell = screen.getByText('first').closest('td');
    expect(cell).not.toBeNull();

    await user.click(cell!);
    expect(screen.getByRole('button', { name: 'Copy value' })).toBeInTheDocument();
    await user.click(cell!);
    expect(screen.queryByRole('button', { name: 'Copy value' })).not.toBeInTheDocument();
  });
});
