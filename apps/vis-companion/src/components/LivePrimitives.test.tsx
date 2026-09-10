// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import fixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/live-primitives.json';
import viewSpec from '../../../../packages/vis-contract/resources/vis-contract/view.json';
import { LiveViewPanel } from './LiveView';
import { applyLivePatch, liveRecordFromText, liveViewFromWire, type LiveNode, type LiveView } from '../lib/live-view';

afterEach(cleanup);
const view = () => liveViewFromWire(fixture)!;
const all = (nodes: LiveNode[]): LiveNode[] => nodes.flatMap(n => n.type === 'group' ? [n, ...all(n.fields)] : [n]);

describe('the complete live vocabulary', () => {
  it('decodes every declared primitive and exposes every heading level', () => {
    const v = view();
    expect(new Set(all(v.nodes).filter(n => n.type !== 'group').map(n => n.type))).toEqual(new Set(viewSpec.live.node_types));
    render(<LiveViewPanel view={v} onActivate={vi.fn()} />);
    for (let level = 1; level <= 6; level++) expect(screen.getByRole('heading', { level })).toBeTruthy();
    expect(document.querySelector('pre code')?.textContent).toBe(fixture.nodes.find(n => n.id === 'code')!.text);
    expect(document.querySelector('pre button')).toBeNull();
    expect(screen.getByRole('link', { name: 'Documentation' })).toHaveProperty('href', 'https://gateway.example.com/docs');
    expect(document.querySelectorAll('.animate-spinner-frame').length).toBe(40);
  });

  it('applies mutable primitive fields without dropping ids, metadata or literal whitespace', () => {
    const changed = applyLivePatch(view(), { view_id: fixture.id, patch: { seq: 1, ops: [
      { op: 'set', node_id: 'h1', text: 'Changed', level: 6 },
      { op: 'set', node_id: 'code', text: '' },
      { op: 'set', node_id: 'dots', variant: 'line', is_active: false },
      { op: 'set', node_id: 'refresh', clicks: 4, is_disabled: true },
    ] } });
    const nodes = all(changed.nodes);
    expect(nodes.find(n => n.id === 'h1')).toMatchObject({ type: 'heading', text: 'Changed', level: 6 });
    expect(nodes.find(n => n.id === 'code')).toMatchObject({ text: '', language: 'python' });
    expect(nodes.find(n => n.id === 'dots')).toMatchObject({ variant: 'line', is_active: false });
    expect(nodes.find(n => n.id === 'refresh')).toMatchObject({ clicks: 4, is_disabled: true });
  });

  it('keeps independent local folds through updates and parent folds, then resets for a receipt', () => {
    // Regression #189: collapsed logs must not reopen when patches arrive.
    let v = view();
    const mounted = render(<LiveViewPanel view={v} />);
    fireEvent.click(screen.getByRole('button', { name: 'Details' }));
    fireEvent.click(screen.getByRole('button', { name: 'Build A logs' }));
    expect(screen.getByText(/A started/)).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Details' }));
    fireEvent.click(screen.getByRole('button', { name: 'Details' }));
    expect(screen.getByText(/A started/)).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Build A logs' }));
    v = applyLivePatch(v, { view_id: v.id, patch: { seq: 1, ops: [{ op: 'append', node_id: 'a', lines: ['A updated'] }] } });
    mounted.rerender(<LiveViewPanel view={v} />);
    expect(screen.queryByText(/A updated/)).toBeNull();
    expect(screen.getByRole('button', { name: 'Build B logs' }).getAttribute('aria-expanded')).toBe('false');
    fireEvent.click(screen.getByRole('button', { name: 'Build A logs' }));
    expect(screen.getByText(/A updated/)).toBeTruthy();
    mounted.rerender(<LiveViewPanel view={v} isSettled />);
    expect(screen.getByRole('button', { name: 'Details' }).getAttribute('aria-expanded')).toBe('false');
    fireEvent.click(screen.getByRole('button', { name: 'Details' }));
    expect(screen.getByRole('button', { name: 'Build A logs' }).getAttribute('aria-expanded')).toBe('false');
    expect(document.querySelector('.animate-spinner-frame')).toBeNull();
    expect(screen.getByRole('button', { name: 'Refresh results' })).toHaveProperty('disabled', true);
  });

  it('uses default_expanded only for an active view and retains logs in a receipt', () => {
    const v: LiveView = { ...view(), nodes: [{ id: 'log', type: 'log', label: 'Logs', default_expanded: true, lines: ['retained'], total_lines: 1, window_lines: 20 }] };
    const mounted = render(<LiveViewPanel view={v} />);
    expect(screen.getByText('retained')).toBeTruthy();
    mounted.rerender(<LiveViewPanel view={v} isSettled />);
    expect(screen.queryByText('retained')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Logs' }));
    expect(screen.getByText('retained')).toBeTruthy();
  });

  it.each(['constructor', 'toString', '__proto__'])('keeps a log named %s collapsed until toggled', (id) => {
    // Regression #189: every valid node id owns its own disclosure state.
    const v: LiveView = { ...view(), nodes: [{ id, type: 'log', label: 'Named log', lines: ['retained'], total_lines: 1, window_lines: 20 }] };
    render(<LiveViewPanel view={v} />);
    const toggle = screen.getByRole('button', { name: 'Named log' });
    expect(toggle.getAttribute('aria-expanded')).toBe('false');
    expect(screen.queryByText('retained')).toBeNull();
    fireEvent.click(toggle);
    expect(screen.getByText('retained')).toBeTruthy();
    fireEvent.click(toggle);
    expect(screen.queryByText('retained')).toBeNull();
  });

  it('waits for one acknowledgement and disables unavailable actions', async () => {
    let finish!: () => void;
    const onActivate = vi.fn(() => new Promise<void>(resolve => { finish = resolve; }));
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
    expect(screen.getByRole('button', { name: 'Refresh results' })).toHaveProperty('disabled', true);
    expect(screen.getByRole('button', { name: 'Details' }).getAttribute('aria-expanded')).toBe('false');
  });
  it('reopens the human record with independent collapsed sections and inert actions', () => {
    // Regression #189: exercise archive decoding, not just a mounted live frame.
    const record = liveRecordFromText([
      JSON.stringify({ kind: 'open', view: fixture }),
      JSON.stringify({ kind: 'close', result: { reason: 'completed', is_completed: true,
        view: { title: fixture.title, nodes: fixture.nodes } } }),
    ].join('\n'))!;
    render(<LiveViewPanel view={record.view} isSettled />);
    expect(screen.getByRole('button', { name: 'Details' }).getAttribute('aria-expanded')).toBe('false');
    fireEvent.click(screen.getByRole('button', { name: 'Details' }));
    expect(screen.queryByText(/A started/)).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Build A logs' }));
    expect(screen.getByText(/A started/)).toBeTruthy();
    expect(screen.queryByText(/B started/)).toBeNull();
    expect(screen.getByRole('button', { name: 'Refresh results' })).toHaveProperty('disabled', true);
    expect(document.querySelector('.animate-spinner-frame')).toBeNull();
  });
});
