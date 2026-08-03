import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';
import {
  DataTable,
  csvNumber,
  filterRows,
  isNumericColumn,
  parseCsv,
  parseTableBlock,
  sortRows,
  toCsv,
  type TableRow,
} from './DataTable';

/** The block `vis_attach` emits for a CSV artifact: five header lines, then the payload. */
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

describe('filter and sort', () => {
  const data = rows([
    ['ada', '9', 'first'],
    ['yak', '10', 'second'],
    ['zed', '120', 'third'],
  ]);

  it('matches any column, case-insensitively; a blank query keeps everything', () => {
    expect(filterRows(data, 'YA').map((r) => r.cells[0])).toEqual(['yak']);
    expect(filterRows(data, 'second').map((r) => r.cells[0])).toEqual(['yak']);
    expect(filterRows(data, '  ').length).toBe(3);
    expect(filterRows(data, 'nope')).toEqual([]);
  });

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

describe('DataTable', () => {
  const html = renderToStaticMarkup(<DataTable body={fence} compact />);

  it('paints a real table with a header row and one row per record', () => {
    expect((html.match(/<tr/g) ?? []).length).toBe(4);
    expect((html.match(/<th\b[^>]*>/g) ?? []).length).toBe(3);
    expect(text(html)).toContain('ada');
    expect(text(html)).toContain('third');
  });

  it('offers the filter, the sort affordance and the row count', () => {
    expect(html).toContain('aria-label="Filter rows of fleet.csv"');
    expect(html).toContain('aria-sort="none"');
    expect(text(html)).toContain('3 rows');
    expect(text(html)).toContain('[Table: fleet.csv 3 rows × 3 cols, 64 B]');
  });

  it('right-aligns the numeric column only', () => {
    const heads = html.match(/<th\b[^>]*>/g) ?? [];
    expect(heads[0]).toContain('text-left');
    expect(heads[1]).toContain('text-right');
    expect(heads[2]).toContain('text-left');
  });

  it('says so instead of painting an empty grid', () => {
    const empty = renderToStaticMarkup(
      <DataTable body={['[Table: none.csv 0 rows × 1 cols, 0 B]', 'none.csv', 'text/csv', '1x0', '0 B', 'name'].join('\n')} compact />,
    );
    expect(text(empty)).toContain('No row matches this filter');
  });
});
