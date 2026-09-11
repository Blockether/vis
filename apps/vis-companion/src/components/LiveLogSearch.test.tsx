// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { LiveViewPanel } from './LiveView';
import type { LiveLogPage, LiveView } from '../lib/live-view';

afterEach(cleanup);
const view = (lines = ['ok', 'ERROR [disk]', 'error again'], total = lines.length): LiveView => ({
  id: 'search-view',
  title: 'Build',
  seq: total,
  nodes: [
    {
      id: 'log',
      type: 'log',
      label: 'Build log',
      lines,
      total_lines: total,
      window_lines: 3,
      default_expanded: true,
    },
  ],
});
const page = (lines = ['ERROR [disk]'], from = 0, matched = 1, total = 500): LiveLogPage => ({
  node_id: 'log',
  from,
  lines,
  matched,
  total,
  line_numbers: lines.map((_, i) => from + i + 10),
});
function search(query: string) {
  const field = screen.getByRole('searchbox', { name: 'Search Build log' });
  fireEvent.change(field, { target: { value: query } });
  fireEvent.submit(field.closest('form')!);
}
const output = () => screen.getByRole('region', { name: 'Build log output' }).textContent;

describe('log search', () => {
  it('matches literal text without case sensitivity and clears with Escape', () => {
    render(<LiveViewPanel view={view()} />);
    search('[DISK]');
    expect(output()).toContain('2: ERROR [disk]');
    expect(output()).not.toContain('error again');
    expect(screen.getByText(/1 matches/)).toBeTruthy();
    fireEvent.keyDown(screen.getByRole('searchbox'), { key: 'Escape' });
    expect(output()).toContain('error again');
    search('.*');
    expect(screen.getByText('No matching lines.')).toBeTruthy();
  });

  it('labels a local-only search when history is unavailable', () => {
    render(<LiveViewPanel view={view(['error visible'], 500)} />);
    search('error');
    expect(screen.getByText(/Loaded lines only/)).toBeTruthy();
    expect(output()).toContain('500: error visible');
  });

  it('searches the durable record and pages by match offset, not source line', async () => {
    const load = vi
      .fn()
      .mockResolvedValueOnce(page(['ERROR [disk]'], 0, 201))
      .mockResolvedValueOnce(page(['error last'], 200, 201));
    render(<LiveViewPanel view={view(['latest'], 500)} load={load} />);
    search('ERROR');
    await waitFor(() => expect(output()).toContain('10: ERROR [disk]'));
    expect(load).toHaveBeenCalledWith('log', 0, 200, 'ERROR');
    expect(screen.getByText(/201 matches.*500 recorded lines/)).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Next matches' }));
    await waitFor(() => expect(output()).toContain('error last'));
    expect(load).toHaveBeenLastCalledWith('log', 200, 200, 'ERROR');
    expect(screen.getByRole('button', { name: 'Next matches' })).toHaveProperty('disabled', true);
  });

  it('ignores an older response after a new query and after clearing', async () => {
    let resolveOld!: (value: LiveLogPage) => void;
    const load = vi
      .fn()
      .mockImplementationOnce(
        () =>
          new Promise<LiveLogPage>((resolve) => {
            resolveOld = resolve;
          }),
      )
      .mockResolvedValueOnce(page(['new match']));
    render(<LiveViewPanel view={view()} load={load} />);
    search('old');
    search('new');
    await waitFor(() => expect(output()).toContain('new match'));
    fireEvent.click(screen.getByRole('button', { name: 'Clear search' }));
    await act(async () => resolveOld(page(['old match'])));
    expect(output()).not.toContain('old match');
    expect(output()).toContain('error again');
  });

  it('reports a failed read and retries the same query', async () => {
    const load = vi.fn().mockRejectedValueOnce(new Error('offline')).mockResolvedValueOnce(page());
    render(<LiveViewPanel view={view()} load={load} />);
    search('error');
    expect(await screen.findByRole('alert')).toHaveProperty(
      'textContent',
      'Could not read log. Try again.',
    );
    fireEvent.click(screen.getByRole('button', { name: 'Refresh results' }));
    await waitFor(() => expect(output()).toContain('ERROR [disk]'));
    expect(load).toHaveBeenLastCalledWith('log', 0, 200, 'error');
  });

  it('keeps a labelled snapshot while new output arrives and refreshes it', async () => {
    const load = vi
      .fn()
      .mockResolvedValueOnce(page(['error before'], 0, 1, 3))
      .mockResolvedValueOnce(page(['error before', 'error after'], 0, 2, 4));
    const rendered = render(<LiveViewPanel view={view()} load={load} />);
    search('error');
    await waitFor(() => expect(output()).toContain('error before'));
    rendered.rerender(<LiveViewPanel view={view(['error after'], 4)} load={load} />);
    expect(screen.getByText('Log changed. Refresh results.')).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Refresh results' }));
    await waitFor(() => expect(output()).toContain('error after'));
    expect(screen.queryByText('Log changed. Refresh results.')).toBeNull();
  });

  it('searches a nested receipt without activating producer controls', async () => {
    const load = vi.fn().mockResolvedValue(page());
    const onActivate = vi.fn();
    const saved = view();
    saved.nodes = [{ id: 'group', type: 'group', direction: 'column', fields: saved.nodes }];
    render(<LiveViewPanel view={saved} load={load} isSettled onActivate={onActivate} />);
    fireEvent.click(screen.getByRole('button', { name: 'Build log' }));
    search('error');
    await waitFor(() => expect(output()).toContain('ERROR [disk]'));
    expect(onActivate).not.toHaveBeenCalled();
  });
});
