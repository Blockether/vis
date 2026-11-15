// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import fixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/live-primitives.json';
import viewSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/view.json';
import { LiveViewPanel } from './LiveView';
import { applyLivePatch, liveViewFromWire, type LiveNode, type LiveView } from '../lib/live-view';

afterEach(cleanup);
const view = () => liveViewFromWire(fixture)!;
const all = (nodes: LiveNode[]): LiveNode[] =>
  nodes.flatMap((n) => (n.type === 'group' ? [n, ...all(n.children)] : [n]));

describe('the complete live vocabulary', () => {
  it('decodes every declared primitive and exposes every heading level', () => {
    expect(
      new Set(
        all(view().nodes)
          .filter((n) => n.type !== 'group')
          .map((n) => n.type),
      ),
    ).toEqual(
      new Set(
        viewSchema.$defs.component.oneOf
          .map((node) => node.properties.type.const)
          .filter((type) => type !== 'group'),
      ),
    );
    render(<LiveViewPanel view={view()} onActivate={vi.fn()} />);
    for (let level = 1; level <= 6; level++)
      expect(screen.getByRole('heading', { level, name: `Heading level ${level}` })).toBeVisible();
    expect(document.querySelector('pre code')?.textContent).toBe(
      fixture.nodes.find((n) => n.id === 'code')!.text,
    );
    expect(document.querySelector('pre button')).toBeNull();
    expect(screen.getByRole('link', { name: 'Documentation' })).toHaveProperty(
      'href',
      'https://gateway.example.com/docs',
    );
    expect(document.querySelectorAll('.animate-spinner-frame').length).toBe(40);
  });

  it('applies mutable primitive fields without dropping ids, metadata or literal whitespace', () => {
    const changed = applyLivePatch(view(), {
      view_id: fixture.id,
      patch: {
        seq: 1,
        ops: [
          { op: 'set', node_id: 'h1', text: 'Changed', level: 6 },
          { op: 'set', node_id: 'code', text: '' },
          { op: 'set', node_id: 'dots', variant: 'line', is_active: false },
          { op: 'set', node_id: 'refresh', clicks: 4, is_disabled: true },
        ],
      },
    });
    const nodes = all(changed.nodes);
    expect(nodes.find((n) => n.id === 'h1')).toMatchObject({
      type: 'heading',
      text: 'Changed',
      level: 6,
    });
    expect(nodes.find((n) => n.id === 'code')).toMatchObject({ text: '', language: 'python' });
    expect(nodes.find((n) => n.id === 'dots')).toMatchObject({ variant: 'line', is_active: false });
    expect(nodes.find((n) => n.id === 'refresh')).toMatchObject({ clicks: 4, is_disabled: true });
  });

  it('paints badges, list items and chart values, and moves them with their patches', () => {
    let v = view();
    const mounted = render(<LiveViewPanel view={v} />);
    const bar = (id: string) =>
      (document.querySelector(`[data-chart-bar="${id}"]`) as HTMLElement).style.width;
    expect(
      [...document.querySelectorAll('[data-badge]')].map((badge) => badge.textContent),
    ).toEqual(['Stable, OK', 'Beta, Warning', 'Docs']);
    expect(screen.getByText('Pin the release').closest('ol')).not.toBeNull();
    expect(screen.getByText('After the checks pass')).toBeVisible();
    // 18 is the greatest value, so its bar fills the scale and 9 fills half of it.
    expect(bar('api:1')).toBe('100%');
    expect(bar('api:2')).toBe('50%');
    v = applyLivePatch(v, {
      view_id: v.id,
      patch: {
        seq: 1,
        ops: [
          { op: 'append', node_id: 'labels', badges: [{ id: 'beta', label: 'RC', tone: 'ok' }] },
          { op: 'remove', node_id: 'labels', item_ids: ['docs'] },
          { op: 'set', node_id: 'todo', is_ordered: false },
          { op: 'append', node_id: 'load', series: [{ id: 'api', label: 'API', values: [36, 18, 9] }] },
          { op: 'remove', node_id: 'load', item_ids: ['web'] },
        ],
      },
    });
    mounted.rerender(<LiveViewPanel view={v} />);
    expect(
      [...document.querySelectorAll('[data-badge]')].map((badge) => badge.textContent),
    ).toEqual(['Stable, OK', 'RC, OK']);
    expect(screen.getByText('Pin the release').closest('ul')).not.toBeNull();
    expect(bar('api:0')).toBe('100%');
    expect(bar('api:1')).toBe('50%');
    expect(document.querySelector('[data-chart-bar^="web"]')).toBeNull();
    v = applyLivePatch(v, {
      view_id: v.id,
      patch: { seq: 2, ops: [{ op: 'set', node_id: 'load', kind: 'line' }] },
    });
    mounted.rerender(<LiveViewPanel view={v} />);
    expect(screen.getByRole('img', { name: 'API 36, 18, 9' })).toBeVisible();
    // A line runs from the greatest value at the top to the least at the bottom.
    expect(document.querySelector('[data-chart-line="api"]')?.getAttribute('points')).toBe(
      '0,4 50,65.33 100,96',
    );
  });

  it('asks for a datetime with the native control of its mode and passes the bounds', () => {
    render(<LiveViewPanel view={view()} />);
    const due = screen.getByLabelText('Due') as HTMLInputElement;
    expect(due.type).toBe('date');
    expect([due.min, due.max, due.value]).toEqual(['2026-01-01', '2026-12-31', '2026-03-14']);
  });

  it('keeps independent local folds through updates and parent folds, then resets for a receipt', () => {
    // Regression #189: collapsed logs must not reopen when patches arrive.
    let v = view();
    const mounted = render(<LiveViewPanel view={v} />);
    fireEvent.click(screen.getByRole('button', { name: 'Details' }));
    fireEvent.click(screen.getByRole('button', { name: 'Build A logs' }));
    expect(screen.getByText(/A started/)).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: 'Details' }));
    fireEvent.click(screen.getByRole('button', { name: 'Details' }));
    expect(screen.getByText(/A started/)).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: 'Build A logs' }));
    v = applyLivePatch(v, {
      view_id: v.id,
      patch: { seq: 1, ops: [{ op: 'append', node_id: 'a', lines: ['A updated'] }] },
    });
    mounted.rerender(<LiveViewPanel view={v} />);
    expect(screen.queryByText(/A updated/)).toBeNull();
    expect(screen.getByRole('button', { name: 'Build B logs' }).getAttribute('aria-expanded')).toBe(
      'false',
    );
    fireEvent.click(screen.getByRole('button', { name: 'Build A logs' }));
    expect(screen.getByText(/A updated/)).toBeVisible();
    mounted.rerender(<LiveViewPanel view={v} isSettled />);
    expect(screen.getByRole('button', { name: 'Details' }).getAttribute('aria-expanded')).toBe(
      'false',
    );
    fireEvent.click(screen.getByRole('button', { name: 'Details' }));
    expect(screen.getByRole('button', { name: 'Build A logs' }).getAttribute('aria-expanded')).toBe(
      'false',
    );
    expect(document.querySelector('.animate-spinner-frame')).toBeNull();
    expect(screen.getByRole('button', { name: 'Refresh results' })).toHaveProperty(
      'disabled',
      true,
    );
  });

  it('uses default_expanded only for an active view and retains logs in a receipt', () => {
    const v: LiveView = {
      ...view(),
      nodes: [
        {
          id: 'log',
          type: 'log',
          label: 'Logs',
          default_expanded: true,
          lines: ['retained'],
          total_lines: 1,
          window_lines: 20,
        },
      ],
    };
    const mounted = render(<LiveViewPanel view={v} />);
    expect(screen.getByText('retained')).toBeVisible();
    mounted.rerender(<LiveViewPanel view={v} isSettled />);
    expect(screen.queryByText('retained')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Logs' }));
    expect(screen.getByText('retained')).toBeVisible();
  });

  it('waits for one acknowledgement and disables unavailable actions', async () => {
    let finish!: () => void;
    const onActivate = vi.fn(
      () =>
        new Promise<void>((resolve) => {
          finish = resolve;
        }),
    );
    render(<LiveViewPanel view={view()} onActivate={onActivate} />);
    const button = screen.getByRole('button', { name: 'Refresh results' });
    fireEvent.click(button);
    fireEvent.click(button);
    fireEvent.click(screen.getByRole('button', { name: 'Unavailable action' }));
    expect(onActivate).toHaveBeenCalledTimes(1);
    expect(onActivate).toHaveBeenCalledWith('refresh');
    expect(button).toHaveProperty('disabled', true);
    finish();
    await waitFor(() => expect(button).toHaveProperty('disabled', false));
  });
  it('keeps completed receipts readable without exposing producer actions', () => {
    render(<LiveViewPanel view={view()} isSettled onInterrupt={vi.fn()} onActivate={vi.fn()} />);
    expect(screen.queryByRole('button', { name: 'Interrupt' })).toBeNull();
    expect(screen.queryByRole('button', { name: 'Select Tests passed' })).toBeNull();
    expect(screen.getByRole('button', { name: 'Refresh results' })).toHaveProperty(
      'disabled',
      true,
    );
    expect(screen.getByRole('button', { name: 'Details' }).getAttribute('aria-expanded')).toBe(
      'false',
    );
  });
});

describe('a timeline and a line of separated items', () => {
  const lanes = () => {
    const node = all(view().nodes).find((n) => n.type === 'timeline');
    if (!node) throw new Error('the fixture holds a timeline');
    return { ...view(), nodes: [node] };
  };
  const start = fixture.nodes.find((n) => n.id === 'timeline')!.lanes!.find((l) => l.id === 'test')!.start!;
  afterEach(() => vi.useRealTimers());

  it('runs a live lane to the clock each second and holds a receipt still', () => {
    vi.useFakeTimers({ toFake: ['Date', 'setInterval', 'clearInterval'] });
    vi.setSystemTime(start + 90_000);
    const mounted = render(<LiveViewPanel view={lanes()} />);
    const row = (label: string) => screen.getByText(label).closest('li')!;
    expect(row('Build')).toHaveTextContent('1m 20s');
    expect(row('Run tests')).toHaveTextContent('1m 30s');
    act(() => vi.advanceTimersByTime(1000));
    expect(row('Run tests')).toHaveTextContent('1m 31s');
    mounted.rerender(<LiveViewPanel view={lanes()} isSettled />);
    expect(row('Run tests')).toHaveTextContent('1m 00s');
    act(() => vi.advanceTimersByTime(5000));
    expect(row('Run tests')).toHaveTextContent('1m 00s');
  });

  it('stands a divider upright only between the items of a wrap line', () => {
    render(
      <LiveViewPanel
        view={{
          id: 'team',
          title: 'Team',
          seq: 1,
          nodes: [
            {
              id: 'head',
              type: 'group',
              direction: 'wrap',
              children: [
                { id: 'team', type: 'paragraph', text: 'TEAM' },
                { id: 'between', type: 'divider' },
                { id: 'time', type: 'paragraph', text: '1m 20s' },
              ],
            },
            { id: 'below', type: 'divider' },
          ],
        }}
      />,
    );
    const [upright, across] = screen.getAllByRole('separator');
    expect(upright.getAttribute('aria-orientation')).toBe('vertical');
    expect(across.tagName).toBe('HR');
    expect(across.getAttribute('aria-orientation')).toBeNull();
  });
});
