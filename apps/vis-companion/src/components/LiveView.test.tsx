// @vitest-environment jsdom
// The panel paints a picture that is being written while it is read, so every
// case here renders the ENGINE's own fixture — the same file
// `gateway/human_input_test.clj` asserts is the engine's projection of a view —
// and reads the document that landed.
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { LiveViewPanel } from './LiveView';
import liveViewSource from './LiveView.tsx?raw';
import fixture from '../lib/live-view.fixture.json';
import { LIVE_NOTE_CHARS, liveViewFromWire, type LiveNode, type LiveView } from '../lib/live-view';

afterEach(cleanup);

function opened(): LiveView {
  const view = liveViewFromWire(fixture);
  if (!view) throw new Error('the engine fixture must be paintable');
  return view;
}

/** The same view with ONE node replaced, wherever a row put it — a run in a different moment. */
function withNode(view: LiveView, node: LiveNode): LiveView {
  const swap = (nodes: LiveNode[]): LiveNode[] =>
    nodes.map((existing) =>
      existing.id === node.id
        ? node
        : existing.type === 'group'
          ? { ...existing, fields: swap(existing.fields) }
          : existing,
    );
  return { ...view, nodes: swap(view.nodes) };
}

function paint(props: Partial<Parameters<typeof LiveViewPanel>[0]> = {}) {
  const { view = opened(), ...rest } = props;
  render(<LiveViewPanel view={view} {...rest} />);
  return document.body.innerHTML;
}

describe('a live view on the phone', () => {
  it('paints every node the run declared, in the order it declared them', () => {
    const html = paint();
    expect(html).toContain('Fleet scan');
    expect(html).toContain('3 hosts · started 12:04');
    const labels = ['Swept', 'Findings', 'Phases', 'Output', 'Hosts', 'Why', 'Elsewhere'];
    expect(labels.map((label) => html.indexOf(label))).toEqual(
      [...labels.map((label) => html.indexOf(label))].sort((a, b) => a - b),
    );
    expect(labels.every((label) => html.includes(label))).toBe(true);

    expect(html).toContain('Scanning db-2');
    expect(html).toContain('host 2 of 3');
    expect(html).toContain('Critical');
    expect(html).toContain('Collect inventory');
    expect(html).toContain('db-2 · 1 critical (openssl)');
    expect(screen.getAllByRole('row').length).toBe(3);
    expect(screen.getByRole('columnheader', { name: 'Findings' }).className).toContain('text-right');
    expect(screen.getByRole('link', { name: 'The run on GitHub' })).toHaveProperty(
      'href',
      'https://example.com/run/42',
    );
  });

  // A section that only paints cannot be heard. The run reports itself, so the
  // panel is a live region: a screen reader is told when the picture moves.
  it('reports itself as work in progress, not as a question', () => {
    paint();
    const panel = screen.getByRole('status');
    expect(panel.getAttribute('aria-live')).toBe('polite');
    expect(screen.queryByRole('dialog')).toBeNull();
  });

  it('draws the fraction as a bar and states the count beside it', () => {
    paint();
    expect(screen.getByRole('progressbar').getAttribute('aria-valuenow')).toBe('67');
    expect(document.body.innerHTML).toContain('2/3');
  });

  // A bar that never moves reads as a stall. A run whose size is not known yet
  // says so in words instead.
  it('draws no bar for a run with no knowable end', () => {
    const html = paint({ view: withNode(opened(), { id: 'swept', type: 'progress', label: 'Swept' }) });
    expect(screen.queryByRole('progressbar')).toBeNull();
    expect(html).toContain('working');
  });

  it("says what an empty node is waiting for, in the engine's own words", () => {
    const html = paint({
      view: withNode(opened(), {
        id: 'hosts',
        type: 'table',
        label: 'Hosts',
        columns: [{ id: 'host', label: 'Host', align: 'left' }],
        rows: [],
        max_rows: 5000,
        order: 'insertion',
      }),
    });
    expect(html).toContain('no rows yet');
    expect(screen.getByRole('columnheader', { name: 'Host' })).toBeTruthy();
  });

  // A path or an attachment names a place on the MACHINE: dressing it as a link
  // would promise a tap that does nothing under the thumb.
  it('opens what the phone can reach and states what it cannot', () => {
    paint();
    expect(screen.queryByRole('link', { name: 'report.md' })).toBeNull();
    expect(document.body.innerHTML).toContain('/tmp/report.md');
  });
});

describe('a log the operator walks back through', () => {
  const behind = (): LiveView =>
    withNode(opened(), {
      id: 'tail',
      type: 'log',
      label: 'Output',
      lines: ['db-2 · 1 critical (openssl)'],
      window_lines: 2000,
      total_lines: 500,
    });

  it('offers the earlier lines only when the record still holds some', () => {
    const html = paint({ view: behind(), load: vi.fn() });
    // What a screen reader hears is the PROMISE; what the eye reads is how much
    // of the run is still behind the window.
    expect(screen.getByRole('button', { name: 'Load 200 earlier lines' })).toBeTruthy();
    expect(html).toContain('499 earlier lines');

    cleanup();
    paint({ view: opened(), load: vi.fn() });
    expect(screen.queryByRole('button', { name: /earlier lines/ })).toBeNull();
  });

  it('reads one page out of the record and keeps it above the window', async () => {
    const load = vi.fn().mockResolvedValue({
      node_id: 'tail',
      from: 299,
      lines: ['db-0 · 0 critical'],
      total: 500,
    });
    paint({ view: behind(), load });

    fireEvent.click(screen.getByRole('button', { name: 'Load 200 earlier lines' }));
    await waitFor(() => expect(document.body.innerHTML).toContain('db-0 · 0 critical'));
    expect(load).toHaveBeenCalledWith('tail', 299, 200);

    const html = document.body.innerHTML;
    expect(html.indexOf('db-0 · 0 critical')).toBeLessThan(html.indexOf('db-2 · 1 critical'));
    // 299 read, 200 fetched, 499 in the record before the window: the lines
    // between what was read and what is on screen are NAMED, never skipped over.
    expect(html).toContain('lines scrolled past while you were reading');
  });
});

describe('stopping the run from the phone', () => {
  it('arms the stop, takes the comment, and sends it with the interrupt', () => {
    const onInterrupt = vi.fn();
    paint({ onInterrupt });
    // Pressing Interrupt STOPS NOTHING yet: it opens the line the reason goes on.
    fireEvent.click(screen.getByRole('button', { name: 'Interrupt' }));
    expect(onInterrupt).not.toHaveBeenCalled();

    const field = screen.getByRole('textbox', { name: 'Why are you stopping Fleet scan?' });
    expect(field).toHaveProperty('maxLength', LIVE_NOTE_CHARS);
    fireEvent.change(field, { target: { value: '  wrong subnet  ' } });
    fireEvent.submit(field.closest('form') as HTMLFormElement);
    expect(onInterrupt).toHaveBeenCalledWith('wrong subnet');
  });

  it('stops with no comment at all — a stop is never held up by one', () => {
    const onInterrupt = vi.fn();
    paint({ onInterrupt });
    fireEvent.click(screen.getByRole('button', { name: 'Interrupt' }));
    fireEvent.submit(
      (screen.getByRole('textbox') as HTMLElement).closest('form') as HTMLFormElement,
    );
    expect(onInterrupt).toHaveBeenCalledWith(null);
  });

  it('keeps watching when the human backs out, and forgets what they typed', () => {
    const onInterrupt = vi.fn();
    paint({ onInterrupt });
    fireEvent.click(screen.getByRole('button', { name: 'Interrupt' }));
    fireEvent.change(screen.getByRole('textbox'), { target: { value: 'never mind' } });
    fireEvent.click(screen.getByRole('button', { name: 'Keep watching' }));
    expect(onInterrupt).not.toHaveBeenCalled();
    expect(screen.queryByRole('textbox')).toBeNull();

    fireEvent.click(screen.getByRole('button', { name: 'Interrupt' }));
    expect(screen.getByRole('textbox')).toHaveProperty('value', '');
  });

  // The key that ARMED the stop is the key that sends it, note and all — the
  // terminal answers Escape the same way, and a comment nobody typed is no
  // comment rather than an empty one.
  it('sends the stop when the human presses Escape over the note', () => {
    const onInterrupt = vi.fn();
    paint({ onInterrupt });
    fireEvent.click(screen.getByRole('button', { name: 'Interrupt' }));
    fireEvent.change(screen.getByRole('textbox'), { target: { value: 'wrong subnet' } });
    fireEvent.keyDown(screen.getByRole('textbox'), { key: 'Escape' });
    expect(onInterrupt).toHaveBeenCalledWith('wrong subnet');
    expect(screen.queryByRole('textbox')).toBeNull();

    fireEvent.click(screen.getByRole('button', { name: 'Interrupt' }));
    fireEvent.keyDown(screen.getByRole('textbox'), { key: 'Escape' });
    expect(onInterrupt).toHaveBeenLastCalledWith(null);
  });

  it('says it is working on it, and offers every view a stop', () => {
    cleanup();
    paint({ onInterrupt: vi.fn(), isInterrupting: true });
    const button = screen.getByRole('button', { name: 'Stopping...' });
    expect(button).toHaveProperty('disabled', true);
  });

  it('states a refusal where the press happened', () => {
    const html = paint({ onInterrupt: vi.fn(), error: 'That view would not stop.' });
    expect(html).toContain('That view would not stop.');
    expect(html).toContain('text-err');
  });
});

describe('what a run says about its own layout', () => {
  // A view lays itself out with the FORM's own group: the row is the run's
  // statement, not the screen's guess, and the terminal splits its band on it.
  it('stands the nodes a row holds side by side', () => {
    paint();
    const list = screen.getByRole('status').querySelector('ul') as HTMLElement;
    // Seven top-level nodes, seven rows: `hosts` and `why` share the one the
    // group holds them in instead of taking one each.
    expect(list.children.length).toBe(7);
    const beside = [...list.children].find((row) => row.textContent?.includes('Hosts')) as HTMLElement;
    expect(beside.textContent).toContain('Why');
    expect(beside.innerHTML).toContain('sm:grid-flow-col');
    // …and the sentence beside a table is set as prose, by the app's ONE rule.
    expect(beside.innerHTML).toContain('text-justify');
    const alone = [...list.children].find((row) => row.textContent?.includes('Elsewhere')) as HTMLElement;
    expect(alone.innerHTML).not.toContain('grid-flow-col');
  });

  // A table is READ across, so every cell is fenced: the eye needs the rail to
  // keep a row together, and the terminal paints the same box.
  it('draws the table as a box with a rule between every pair of rows', () => {
    paint();
    const table = screen.getByRole('table');
    expect(table.className).toContain('border border-dialog-edge');
    const cells = [...table.querySelectorAll('th, td')];
    // Three columns over a header and two rows: nine cells, every one fenced.
    expect(cells.length).toBe(9);
    expect(cells.every((cell) => cell.className.includes('border border-dialog-edge'))).toBe(true);
  });

  it('paints the marks a human wrote, and nothing a block would bring', () => {
    const panel = (paint(), screen.getByRole('status'));
    expect([...panel.querySelectorAll('code')].map((mark) => mark.textContent)).toContain('db-2');
    expect([...panel.querySelectorAll('strong')].map((mark) => mark.textContent)).toContain(
      'openssl 3.0.13',
    );
    // A row stays a row: no heading, no quote, no list arrived with the marks.
    expect(panel.querySelector('h1, h2, h3, blockquote')).toBeNull();
  });

  it('marks a table cell too, because a cell is a human string as well', () => {
    const view = opened();
    const hosts = view.nodes
      .flatMap((node) => (node.type === 'group' ? node.fields : [node]))
      .find((node) => node.id === 'hosts');
    if (!hosts || hosts.type !== 'table') throw new Error('the fixture must hold the hosts table');
    paint({
      view: withNode(view, { ...hosts, rows: [{ ...hosts.rows[0], cells: ['`db-1`', 'clean', '0'] }] }),
    });
    expect(screen.getAllByRole('cell')[0].querySelector('code')?.textContent).toBe('db-1');
  });
});

describe('the section is built from the closed vocabulary', () => {
  it("borrows the app's controls and writes no styles of its own", () => {
    expect(liveViewSource).toContain('<Button');
    expect(liveViewSource).toContain('<Meter');
    expect(liveViewSource).toContain('<LoadMore');
    expect(liveViewSource).toContain('<Spinner');
    expect(liveViewSource).not.toContain('<button');
    expect(liveViewSource).not.toContain('style={');
    expect(liveViewSource).not.toContain('style="');
  });
});
