// @vitest-environment jsdom
// The panel paints a picture that is being written while it is read, so every
// case here renders the ENGINE's own fixture — the same file
// `gateway/human_input_test.clj` asserts is the engine's projection of a view —
// and reads the document that landed.
import { act, cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { LiveView as LiveViewList, LiveViewPanel, useLiveViews } from './LiveView';
import liveViewSource from './LiveView.tsx?raw';
import fixture from '../lib/live-view.fixture.json';
import type { GatewayClient } from '../lib/gateway';
import {
  applyLivePatch,
  LIVE_NOTE_CHARS,
  liveViewFromWire,
  type LiveNode,
  type LiveView,
} from '../lib/live-view';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import type { SseEvent } from '../lib/types';
import { VIEW_CLOSE_EVENT, VIEW_PATCH_EVENT } from '../lib/view';

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

describe('live log disclosures', () => {
  // Regression #189: a scrollable log is not collapsed output.
  it('starts each build log collapsed without deleting its retained output', () => {
    const view: LiveView = {
      ...opened(),
      nodes: [
        {
          id: 'a',
          type: 'log',
          label: 'Build A logs',
          lines: ['A retained'],
          window_lines: 100,
          total_lines: 1,
        },
        {
          id: 'b',
          type: 'log',
          label: 'Build B logs',
          lines: ['B retained'],
          window_lines: 100,
          total_lines: 1,
        },
      ],
    };
    paint({ view });
    expect(screen.queryByText('A retained')).toBeNull();
    expect(screen.queryByText('B retained')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Build A logs' }));
    expect(screen.getByText('A retained')).toBeVisible();
    expect(screen.queryByText('B retained')).toBeNull();
    expect(view.nodes[0]).toHaveProperty('lines', ['A retained']);
  });
});

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
    fireEvent.click(screen.getByRole('button', { name: 'Output' }));
    expect(document.body.innerHTML).toContain('db-2 · 1 critical (openssl)');
    expect(screen.getByLabelText('Output output').getAttribute('tabindex')).toBe('0');
    expect(screen.getAllByRole('row').length).toBe(3);
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

  it('does not repeat the default live state as a badge', () => {
    paint();
    expect(within(screen.getByRole('banner')).queryByText('Live')).toBeNull();
  });

  it('draws the fraction as a bar and states the count beside it', () => {
    paint();
    expect(screen.getByRole('progressbar').getAttribute('aria-valuenow')).toBe('67');
    expect(document.body.innerHTML).toContain('2/3');
  });

  // A bar that never moves reads as a stall. A run whose size is not known yet
  // says so in words instead.
  it('draws no bar for a run with no knowable end', () => {
    const html = paint({
      view: withNode(opened(), { id: 'swept', type: 'progress', label: 'Swept' }),
    });
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
        is_selectable: false,
        selected_ids: [],
        groups: [],
      }),
    });
    expect(html).toContain('no rows yet');
    expect(screen.getByRole('columnheader', { name: 'Host' })).toBeVisible();
  });

  // Weight is for the NAME, and caps are for no one's words but ours. A live node
  // names what it is a label OF — the failing job, the build a console belongs to —
  // and those words are the extension's, so they keep the case it wrote them in.
  it('sets the node name apart by weight and keeps the case its author wrote', () => {
    paint({
      view: withNode(opened(), {
        id: 'tail',
        type: 'log',
        label: 'Failure · Run native build',
        lines: ['exit 1'],
        window_lines: 2000,
        total_lines: 1,
      }),
    });
    const name = [...document.querySelectorAll('span')].find(
      (one) => one.textContent === 'Failure',
    )!;
    expect(name.className).toContain('font-bold');
    expect(name.className).not.toContain('uppercase');
    expect(name.parentElement?.textContent).toBe('Failure · Run native build');
  });

  // A rule fences one block of a run off from the next. A divider is a rule the view
  // ASKED for, so the rows on either side of one are not ruled a second time.
  it('rules the blocks of a run apart without doubling a divider', () => {
    const view: LiveView = {
      ...opened(),
      nodes: [
        { id: 'queued', type: 'paragraph', text: 'Build queued.' },
        { id: 'left', type: 'paragraph', text: 'Two jobs left.' },
        { id: 'break', type: 'divider' },
        {
          id: 'hosts',
          type: 'table',
          label: 'Hosts',
          columns: [{ id: 'host', label: 'Host', align: 'left' }],
          rows: [],
          max_rows: 5000,
          order: 'insertion',
          is_selectable: false,
          selected_ids: [],
          groups: [],
        },
        { id: 'written', type: 'paragraph', text: 'Report written.' },
      ],
    };
    paint({ view });
    const rows = [...document.querySelectorAll('section > ul > li')];
    expect(rows.map((one) => one.className.includes('border-t'))).toEqual([
      false,
      true,
      false,
      false,
      true,
    ]);
  });
  // A path or an attachment names a place on the MACHINE: dressing it as a link
  // would promise a tap that does nothing under the thumb.
  it('opens what the phone can reach and states what it cannot', () => {
    paint();
    expect(screen.queryByRole('link', { name: 'report.md' })).toBeNull();
    expect(document.body.innerHTML).toContain('/tmp/report.md');
  });
});

describe('selecting a table row', () => {
  const selectableView = (): LiveView => {
    const view = opened();
    const hosts = view.nodes
      .flatMap((node) => (node.type === 'group' ? node.fields : [node]))
      .find((node) => node.id === 'hosts');
    if (!hosts || hosts.type !== 'table') throw new Error('the fixture must hold the hosts table');
    return withNode(view, {
      ...hosts,
      is_selectable: true,
      selected_ids: ['db-1', 'db-2'],
    });
  };

  it('shows every default selection and sends a press anywhere in the job row', () => {
    const onSelect = vi.fn();
    paint({ view: selectableView(), onSelect });

    const first = screen.getByRole('button', { name: 'Select db-1' });
    const second = screen.getByRole('button', { name: 'Select db-2' });
    expect(first.getAttribute('aria-pressed')).toBe('true');
    expect(second.getAttribute('aria-pressed')).toBe('true');
    fireEvent.click(second);
    expect(onSelect).toHaveBeenCalledWith('hosts', ['db-2']);

    onSelect.mockClear();
    // A middle column appears twice in the DOM on purpose — stacked under the name
    // for a phone, and as its own cell from `sm` — and never both on one screen. The
    // press has to reach the row from the cell that is NOT the button.
    const spelled = screen.getAllByText('critical');
    fireEvent.click(spelled[spelled.length - 1]);
    expect(onSelect).toHaveBeenCalledWith('hosts', ['db-2']);
  });
  /** The hosts table of a view, which sits one level down when the view groups it. */
  const hostsTable = (view: LiveView) => {
    const node = view.nodes
      .flatMap((one) => (one.type === 'group' ? one.fields : [one]))
      .find((one) => one.id === 'hosts');
    if (!node || node.type !== 'table') throw new Error('the fixture must hold the hosts table');
    return node;
  };

  /** One matrix parent with two variants, beside a job that stands alone. */
  const matrixView = (selectedIds: string[]): LiveView => {
    const view = selectableView();
    const hosts = hostsTable(view);
    hosts.rows = [
      { id: 'ios', cells: ['iOS', 'queued'], tone: 'running', parent: 'Release apps' },
      { id: 'android', cells: ['Android', 'running'], tone: 'running', parent: 'Release apps' },
      { id: 'docs', cells: ['Publish docs', 'success'], tone: 'ok' },
    ];
    hosts.selected_ids = selectedIds;
    return view;
  };

  // Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: matrix jobs repeated
  // their whole parent name as flat peers, making the run hard to scan on a phone.
  it('nests matrix jobs under a parent that starts collapsed', () => {
    paint({ view: matrixView(['android']), onSelect: vi.fn() });

    const parent = screen.getByRole('button', { name: 'Release apps' });
    expect(parent.getAttribute('aria-expanded')).toBe('false');
    // The head counts its own legs; the producer names the parent and nothing else.
    expect(screen.getByText('2 rows')).toBeVisible();
    expect(screen.queryByRole('button', { name: 'Select iOS' })).toBeNull();
    expect(screen.getByRole('button', { name: 'Select Publish docs' })).toBeVisible();

    fireEvent.click(parent);
    expect(parent.getAttribute('aria-expanded')).toBe('true');
    expect(screen.getByRole('button', { name: 'Select iOS' })).toBeVisible();
    expect(screen.getByText('Android')).toBeVisible();
  });
  // Regression, reported from the phone: a long job name ran straight into the
  // count beside it, so a head read as one run-on line and the name gave way
  // right up against the number.
  it('keeps a group head name off the count it carries', () => {
    const view = matrixView([]);
    const parent = 'Verify release source / python-package / ubuntu-latest';
    hostsTable(view).rows = [
      { id: 'ios', cells: ['iOS', 'queued'], tone: 'running', parent },
      { id: 'android', cells: ['Android', 'running'], tone: 'running', parent },
    ];
    paint({ view, onSelect: vi.fn() });

    const name = screen.getByText(parent);
    const count = screen.getByText('2 rows');
    // The name is the only part that may give way, and it stops short of the count.
    expect(name.className).toContain('truncate');
    expect(count.className).toContain('shrink-0');
    expect(count.className).toContain('pl-2');
  });
  // Regression, session 641fbdc0-44a9-46dd-86c9-3e8b9bdf878b: a parent with a single leg
  // stayed flat beside its folded neighbors, so one table showed two shapes and the lone
  // rows were the only ones still repeating their parent's name.
  it('folds a parent that has one leg like every other parent', () => {
    const view = matrixView([]);
    const hosts = hostsTable(view);
    hosts.rows = [
      ...hosts.rows.slice(0, 2),
      { id: 'docs', cells: ['Publish docs', 'success'], tone: 'ok', parent: 'Lint' },
    ];
    paint({ view, onSelect: vi.fn() });

    const parent = screen.getByRole('button', { name: 'Lint' });
    expect(parent.getAttribute('aria-expanded')).toBe('false');
    expect(screen.getByText('1 row')).toBeVisible();
    expect(screen.queryByRole('button', { name: 'Select Publish docs' })).toBeNull();

    fireEvent.click(parent);
    expect(screen.getByRole('button', { name: 'Select Publish docs' })).toBeVisible();
  });
  // Regression, session c6473f43-3b3b-48f0-b309-64b7b37e8a21: every poll re-sent the
  // running jobs as the selection, which sprang open the parent the reader had closed.
  it('keeps a parent closed until the reader opens it, poll after poll', () => {
    const onSelect = vi.fn();
    const { rerender } = render(
      <LiveViewPanel view={matrixView(['android'])} onSelect={onSelect} />,
    );

    const parent = screen.getByRole('button', { name: 'Release apps' });
    fireEvent.click(parent);
    expect(parent.getAttribute('aria-expanded')).toBe('true');
    fireEvent.click(parent);

    rerender(<LiveViewPanel view={matrixView(['ios'])} onSelect={onSelect} />);
    expect(screen.getByRole('button', { name: 'Release apps' }).getAttribute('aria-expanded')).toBe(
      'false',
    );
    expect(screen.queryByRole('button', { name: 'Select iOS' })).toBeNull();
  });
  // Regression, session c6473f43-3b3b-48f0-b309-64b7b37e8a21: GitHub lists a matrix
  // interleaved with the rest of the run, so a parent head showed the first leg it met
  // and the remaining legs stood in the table as rows belonging to no parent.
  it('gathers a parent legs under its own head, however the run interleaves them', () => {
    const view = matrixView([]);
    const hosts = hostsTable(view);
    hosts.rows = [
      { id: 'ios', cells: ['iOS', 'queued'], tone: 'running', parent: 'Release apps' },
      { id: 'docs', cells: ['Publish docs', 'success'], tone: 'ok' },
      { id: 'android', cells: ['Android', 'running'], tone: 'running', parent: 'Release apps' },
    ];
    paint({ view, onSelect: vi.fn() });

    expect(screen.queryByRole('button', { name: 'Select Android' })).toBeNull();

    fireEvent.click(screen.getByRole('button', { name: 'Release apps' }));
    expect(screen.getByRole('button', { name: 'Select iOS' })).toBeVisible();
    expect(screen.getByRole('button', { name: 'Select Android' })).toBeVisible();

    const order = screen.getAllByRole('row').map((row) => row.textContent ?? '');
    const at = (text: string) => order.findIndex((line) => line.includes(text));
    expect(at('Release apps')).toBeLessThan(at('iOS'));
    expect(at('iOS')).toBeLessThan(at('Android'));
    expect(at('Android')).toBeLessThan(at('Publish docs'));
  });
  // A DECLARED group is that same fold given an identity: the producer owns the order
  // the heads paint in, the label each head wears and the tone that says a leg failed
  // before anybody opens the fold.
  it('paints declared groups in the declared order, wearing the label and tone they declared', () => {
    const view = matrixView([]);
    const hosts = hostsTable(view);
    hosts.rows = [
      { id: 'ios', cells: ['iOS', 'queued'], tone: 'running', parent: 'release' },
      { id: 'docs', cells: ['Publish docs', 'failed'], tone: 'error', parent: 'publish' },
    ];
    hosts.groups = [
      { id: 'release', label: 'Release apps', order: 1 },
      { id: 'publish', label: 'Publish', order: 0, tone: 'error' },
    ];
    paint({ view, onSelect: vi.fn() });

    const order = screen.getAllByRole('row').map((row) => row.textContent ?? '');
    const at = (text: string) => order.findIndex((line) => line.includes(text));
    expect(at('Publish')).toBeLessThan(at('Release apps'));
    // The head wears the LABEL; the id its rows point at never reaches the screen.
    expect(screen.queryByText('release')).toBeNull();
    expect(screen.getByRole('button', { name: 'Publish' }).innerHTML).toContain('text-err');
    expect(screen.getByRole('button', { name: 'Release apps' }).innerHTML).not.toContain('text-err');
  });
  it('opens a group the producer declared open, and lets the reader shut it again', () => {
    const view = matrixView([]);
    const hosts = hostsTable(view);
    hosts.rows = [{ id: 'ios', cells: ['iOS', 'queued'], tone: 'running', parent: 'release' }];
    hosts.groups = [{ id: 'release', label: 'Release apps', is_open: true }];
    paint({ view, onSelect: vi.fn() });

    const head = screen.getByRole('button', { name: 'Release apps' });
    expect(head.getAttribute('aria-expanded')).toBe('true');
    fireEvent.click(head);
    expect(
      screen.getByRole('button', { name: 'Release apps' }).getAttribute('aria-expanded'),
    ).toBe('false');
  });
  // Expansion used to be keyed on the label, so renaming a group shut the fold under
  // the reader. It is keyed on the group id, which a rename does not touch.
  it('keeps a group open when the producer renames it mid-run', () => {
    const grouped = (label: string): LiveView => {
      const view = matrixView([]);
      const hosts = hostsTable(view);
      hosts.rows = [{ id: 'ios', cells: ['iOS', 'queued'], tone: 'running', parent: 'release' }];
      hosts.groups = [{ id: 'release', label }];
      return view;
    };
    const { rerender } = render(<LiveViewPanel view={grouped('Release apps')} onSelect={vi.fn()} />);

    fireEvent.click(screen.getByRole('button', { name: 'Release apps' }));
    rerender(<LiveViewPanel view={grouped('Ship it')} onSelect={vi.fn()} />);

    expect(screen.getByRole('button', { name: 'Ship it' }).getAttribute('aria-expanded')).toBe(
      'true',
    );
    expect(screen.getByRole('button', { name: 'Select iOS' })).toBeVisible();
  });
  // Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: selecting a job only
  // tinted its first cell, so the table looked like a cell picker instead of a row picker.
  it('paints the selected state across the whole job row', () => {
    paint({ view: selectableView(), onSelect: vi.fn() });

    const selected = screen.getByRole('button', { name: 'Select db-2' });
    const row = selected.closest('tr') as HTMLTableRowElement;
    expect(row.getAttribute('aria-selected')).toBe('true');
  });

  it('sends the row through the shared View action instead of keeping private selection', () => {
    const view = selectableView();
    const viewAction = vi.fn(async () => ({
      action: 'select' as const,
      is_accepted: true,
      item_ids: ['db-2'],
      node_id: 'hosts',
      view_id: view.id,
    }));
    const client = { viewAction } as unknown as GatewayClient;

    render(<LiveViewList views={[view]} client={client} sid="session-1" />);
    fireEvent.click(screen.getByRole('button', { name: 'Select db-2' }));

    expect(viewAction).toHaveBeenCalledWith('session-1', view.id, {
      action: 'select',
      node_id: 'hosts',
      item_ids: ['db-2'],
    });
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
      default_expanded: true,
    });

  it('offers the earlier lines only when the record still holds some', () => {
    const html = paint({ view: behind(), load: vi.fn() });
    // What a screen reader hears is the PROMISE; what the eye reads is how much
    // of the run is still behind the window.
    expect(screen.getByRole('button', { name: 'Load 200 earlier lines' })).toBeVisible();
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
  // Regression: the live header must use the shared compact face, not a 44px slab.
  it('keeps the interrupt face compact while preserving touch reach', () => {
    paint({ onInterrupt: vi.fn() });
    const button = screen.getByRole('button', { name: 'Interrupt' });
    expect(button.classList.contains('min-h-11')).toBe(false);
    expect(button.classList.contains('after:absolute')).toBe(true);
  });
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
    const beside = [...list.children].find((row) =>
      row.textContent?.includes('Hosts'),
    ) as HTMLElement;
    expect(beside.textContent).toContain('Why');
    expect(beside.querySelector('[data-view-layout="row"]')).toBeInTheDocument();
    const alone = [...list.children].find((row) =>
      row.textContent?.includes('Elsewhere'),
    ) as HTMLElement;
    expect(alone.querySelector('[data-view-layout]')).toBeNull();
  });

  // A phone reads a table DOWN its first column. The row's own line carries the
  // name and, at the right edge, the value it is measured by; every column between
  // them stacks under the name, and they take their cells back only when there is
  // width for them. Forty fenced cells at 8px were a grid to decode before a run
  // could be read — a rule between rows is all the eye needs to keep a row whole.
  it('reads down a phone: no fenced cells, a rule between rows, columns only at sm', () => {
    paint();
    const table = screen.getByRole('table');
    expect(table.className).not.toContain('border border-dialog-edge');
    expect(table.querySelector('tbody')?.className).toContain('divide-y');
    const head = table.querySelector('thead') as HTMLElement;
    expect(head.className).toContain('sm:table-header-group');
    const cells = [...table.querySelectorAll('td')];
    expect(cells.some((cell) => cell.className.includes('border border-dialog-edge'))).toBe(false);
    const [name, ...rest] = [...(table.querySelector('tbody tr') as HTMLElement).children];
    expect(name.className).not.toContain('hidden');
    expect(
      rest.every(
        (cell) => cell.className.includes('hidden') && cell.className.includes('sm:table-cell'),
      ),
    ).toBe(true);
  });

  it('keeps the name and its value on one line and stacks the rest beneath', () => {
    const view = opened();
    const hosts = view.nodes
      .flatMap((node) => (node.type === 'group' ? node.fields : [node]))
      .find((node) => node.id === 'hosts');
    if (!hosts || hosts.type !== 'table') throw new Error('the fixture must hold the hosts table');
    paint({
      view: withNode(view, {
        ...hosts,
        columns: [
          { id: 'job', label: 'Job', align: 'left' },
          { id: 'now', label: 'Now', align: 'left' },
          { id: 'took', label: 'Took', align: 'right' },
        ],
        rows: [{ ...hosts.rows[0], cells: ['macos-latest', 'Run native build', '44m46s'] }],
      }),
    });
    const face = screen.getAllByRole('cell')[0] as HTMLElement;
    expect(face.textContent).toContain('macos-latest');
    expect(face.textContent).toContain('44m46s');
    expect(face.textContent).toContain('Run native build');
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
      view: withNode(view, {
        ...hosts,
        rows: [{ ...hosts.rows[0], cells: ['`db-1`', 'clean', '0'] }],
      }),
    });
    expect(screen.getAllByRole('cell')[0].querySelector('code')?.textContent).toBe('db-1');
  });

  // Regression, reported from the phone: the headline shared one flex line with
  // its detail, so "1 of 2 jobs finished" beside a workflow and a job name was
  // squeezed to one word per line — a column of five words down the panel.
  it('gives the headline the whole width and puts its detail on the line below', () => {
    paint();
    const headline = screen.getByText('Scanning db-2').closest('p') as HTMLElement;
    const detail = screen.getByText('host 2 of 3').closest('p') as HTMLElement;
    // Two elements, not two halves of one line.
    expect(headline).not.toBe(detail);
    expect(headline.contains(detail)).toBe(false);
    // The same grid, the detail on its own row under the headline's column.
    expect(detail.parentElement).toBe(headline.parentElement);
    expect(detail.className).toContain('col-start-2');
    // Nothing may take width from the headline any more.
    expect(headline.className).not.toContain('flex-1');
    expect(headline.parentElement?.className).not.toContain('flex');
  });


  // Regression, reported from the phone: a step's detail is the part that can be
  // any length, and it refused to shrink — so a long one pushed the row past the
  // screen instead of saying a little less.
  it('trims a long step detail instead of stretching its row', () => {
    const view: LiveView = {
      ...opened(),
      nodes: [
        {
          id: 'timeline',
          type: 'steps',
          steps: [
            {
              id: 'verify',
              label: 'Verify release source',
              detail: 'python-package / ubuntu-latest / real SDK engine, waiting for a runner',
              tone: 'running',
            },
          ],
        },
      ],
    };
    paint({ view });

    const detail = screen.getByText(/real SDK engine/).closest('span') as HTMLElement;
    expect(detail.className).toContain('truncate');
    expect(detail.className).toContain('min-w-0');
    expect(detail.className).not.toContain('shrink-0');
  });
  // Regression, user report: Activity patches rendered one-by-one on WKWebView and
  // starved the independent elapsed-time paint, leaving its clock visibly frozen.
  it('coalesces a patch burst into one phone paint', async () => {
    let receive: ((event: SseEvent) => void) | null = null;
    const running = opened();
    const client = { liveViews: () => Promise.resolve([running]) } as unknown as GatewayClient;
    const subscriptions = {
      subscribeConnection: () => () => undefined,
      subscribeSession: (_sid: string, listener: (event: SseEvent) => void) => {
        receive = listener;
        return () => undefined;
      },
    } as unknown as SessionSubscriptionHub;
    function Probe() {
      const views = useLiveViews(client, subscriptions, 'session-1');
      return <span>{views[0]?.seq ?? 'none'}</span>;
    }

    render(<Probe />);
    await waitFor(() => expect(screen.getByText('0')).toBeVisible());
    vi.useFakeTimers();
    try {
      for (const seq of [1, 2, 3]) {
        act(() =>
          receive?.({
            type: VIEW_PATCH_EVENT,
            kind: 'live',
            view_id: running.id,
            first_seq: seq,
            patch: { view_id: running.id, seq, ops: [] },
          }),
        );
      }

      expect(screen.getByText('0')).toBeVisible();
      act(() => vi.advanceTimersByTime(100));
      expect(screen.getByText('3')).toBeVisible();
    } finally {
      vi.useRealTimers();
    }
  });

  // Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: interrupt
  // removed the live panel, then its one transcript read raced the running block
  // persisting the record, so no artifact row appeared.
  it('re-reads through the handoff from a closed view to its transcript artifact', async () => {
    let receive: ((event: SseEvent) => void) | null = null;
    const client = { liveViews: () => Promise.resolve([opened()]) } as unknown as GatewayClient;
    const subscriptions = {
      subscribeConnection: () => () => undefined,
      subscribeSession: (_sid: string, listener: (event: SseEvent) => void) => {
        receive = listener;
        return () => undefined;
      },
    } as unknown as SessionSubscriptionHub;
    const onRecordFiled = vi.fn();

    function Probe() {
      const views = useLiveViews(client, subscriptions, 'session-1', onRecordFiled);
      return <span>{views.length}</span>;
    }

    render(<Probe />);
    await waitFor(() => expect(screen.getByText('1')).toBeVisible());
    vi.useFakeTimers();
    try {
      act(() =>
        receive?.({
          type: VIEW_CLOSE_EVENT,
          kind: 'live',
          view_id: opened().id,
          result: { artifact_id: 'record-1' },
        }),
      );

      expect(onRecordFiled).toHaveBeenCalledTimes(1);
      expect(screen.getByText('0')).toBeVisible();
      act(() => vi.advanceTimersByTime(8_000));
      expect(onRecordFiled).toHaveBeenCalledTimes(5);
    } finally {
      vi.useRealTimers();
    }
  });

  // The settled-Activity-close cases that stood here are gone with the lifecycle
  // they tested: protocol 7 keeps no view alive past its own close, because the
  // form that produced the work carries the terminal snapshot.
  it('repairs a sequence gap from the materialized snapshot', async () => {
    let receive: ((event: SseEvent) => void) | null = null;
    const running = opened();
    const repaired = { ...running, seq: 4 };
    const liveViews = vi
      .fn<() => Promise<LiveView[]>>()
      .mockResolvedValueOnce([running])
      .mockResolvedValue([repaired]);
    const client = { liveViews } as unknown as GatewayClient;
    const subscriptions = {
      subscribeConnection: () => () => undefined,
      subscribeSession: (_sid: string, listener: (event: SseEvent) => void) => {
        receive = listener;
        return () => undefined;
      },
    } as unknown as SessionSubscriptionHub;

    function Probe() {
      const views = useLiveViews(client, subscriptions, 'session-1');
      return <span>{views[0]?.seq ?? 'none'}</span>;
    }

    render(<Probe />);
    await waitFor(() => expect(screen.getByText('0')).toBeVisible());
    act(() =>
      receive?.({
        type: VIEW_PATCH_EVENT,
        kind: 'live',
        view_id: running.id,
        first_seq: 3,
        patch: { view_id: running.id, seq: 4, ops: [] },
      }),
    );
    await waitFor(() => expect(screen.getByText('4')).toBeVisible());
    expect(liveViews).toHaveBeenCalledTimes(2);
  });

  it('replaces a view with its reconnect snapshot', async () => {
    let reconnect: ((connected: boolean) => void) | null = null;
    const running = opened();
    const refreshed = { ...running, seq: 2 };
    const liveViews = vi
      .fn<() => Promise<LiveView[]>>()
      .mockResolvedValueOnce([running])
      .mockResolvedValue([refreshed]);
    const client = { liveViews } as unknown as GatewayClient;
    const subscriptions = {
      subscribeConnection: (listener: (connected: boolean) => void) => {
        reconnect = listener;
        return () => undefined;
      },
      subscribeSession: () => () => undefined,
    } as unknown as SessionSubscriptionHub;

    function Probe() {
      const views = useLiveViews(client, subscriptions, 'session-1');
      return <span>{views[0]?.seq ?? 'none'}</span>;
    }

    render(<Probe />);
    await waitFor(() => expect(screen.getByText('0')).toBeVisible());
    act(() => reconnect?.(true));
    await waitFor(() => expect(screen.getByText('2')).toBeVisible());
    expect(liveViews).toHaveBeenCalledTimes(2);
  });
});

describe('the section is built from the closed vocabulary', () => {
  it('uses shared actions around its feature-only meter and tables', () => {
    expect(liveViewSource).toContain('<Button');
    expect(liveViewSource).toContain('<ProgressMeter');
    expect(liveViewSource).toContain('<LoadMore');
    expect(liveViewSource).toContain('<Disclosure');
    // Motion is explicit: only a declared spinner uses the shared control.
    expect(liveViewSource).toContain('<Spinner');
    expect(liveViewSource).not.toContain('<button');
    expect(liveViewSource).not.toContain('style={');
    expect(liveViewSource).not.toContain('style="');
  });
});

// A run used to open in the artifact overlay, which on a desktop papered the session list,
// the transcript and the composer with one view. It opens in the app's ONE dialog now, and
// that dialog stands in the session's own pane rather than over the whole window.
describe('an opened run is a dialog over the chat', () => {
  it('opens in `Modal` + `DialogFrame`, never the full-screen artifact overlay', () => {
    expect(liveViewSource).toContain('<Modal within="session" onDismiss={onClose}>');
    expect(liveViewSource).toContain('<DialogFrame title={title} subtitle={subtitle} onClose={onClose}>');
    expect(liveViewSource).not.toContain('OverlayScreen');
  });
});

describe('live horizontal dividers', () => {
  it.each([false, true])('retains a semantic noninteractive divider (settled=%s)', (isSettled) => {
    const view = liveViewFromWire({
      ...fixture,
      nodes: [
        { id: 'before', type: 'paragraph', text: 'Build completed' },
        { id: 'results-break', type: 'divider' },
        { id: 'after', type: 'paragraph', text: 'Review the results' },
      ],
    });
    expect(view?.nodes.map((node) => node.id)).toEqual(['before', 'results-break', 'after']);
    paint({ view: view!, isSettled });
    const divider = screen.getByRole('separator');
    expect(divider.tagName).toBe('HR');
    expect(divider.tabIndex).toBe(-1);
    expect(divider.textContent).toBe('');
    expect(
      screen.getByText('Build completed').compareDocumentPosition(divider) &
        Node.DOCUMENT_POSITION_FOLLOWING,
    ).toBe(Node.DOCUMENT_POSITION_FOLLOWING);
    expect(
      divider.compareDocumentPosition(screen.getByText('Review the results')) &
        Node.DOCUMENT_POSITION_FOLLOWING,
    ).toBe(Node.DOCUMENT_POSITION_FOLLOWING);
  });
  it('adds and removes a divider without allowing content patches to mutate it', () => {
    let view = liveViewFromWire({ ...fixture, nodes: [{ id: 'break', type: 'divider' }] })!;
    const divider = { id: 'break', type: 'divider' };
    const update = (ops: unknown[]) => {
      const seq = view.seq + 1;
      view = applyLivePatch(view, {
        type: VIEW_PATCH_EVENT,
        kind: 'live',
        view_id: view.id,
        first_seq: seq,
        patch: { view_id: view.id, seq, ops },
      });
    };
    for (const op of ['set', 'append', 'remove', 'clear']) {
      update([{ op, node_id: 'break', label: 'Not a heading', text: 'Not prose' }]);
      expect(view.nodes).toEqual([divider]);
    }
    update([{ op: 'add-node', after: 'break', node_spec: { id: 'second', type: 'divider' } }]);
    expect(view.nodes).toEqual([divider, { id: 'second', type: 'divider' }]);
    update([{ op: 'remove-node', node_id: 'break' }]);
    expect(view.nodes).toEqual([{ id: 'second', type: 'divider' }]);
  });
});
