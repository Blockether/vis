// @vitest-environment jsdom
// Live views outside the transcript: inputs and cards, and the side panel.
import { act, cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { LIVE_CHANGE_DEBOUNCE_MS, LiveView as LiveViews, LiveViewPanel } from './LiveView';
import { LivePanel, ResizablePanel, initialPanelWidth } from './LivePanel';
import type { GatewayClient } from '../lib/gateway';
import { liveInputNodes, liveViewFromWire, livePlacement, type LiveView } from '../lib/live-view';

afterEach(() => {
  cleanup();
  vi.useRealTimers();
});

function view(wire: Record<string, unknown>): LiveView {
  const parsed = liveViewFromWire({ seq: 0, title: 'Board', ...wire });
  if (!parsed) throw new Error('the view must be paintable');
  return parsed;
}

const form = () =>
  view({
    id: 'form',
    placement: 'panel',
    extension: 'deploy',
    nodes: [
      { id: 'name', type: 'plaintext', label: 'Name', value: 'Ada' },
      { id: 'token', type: 'password', label: 'Token', is_set: true },
      { id: 'go', type: 'button', label: 'Start', action: 'submit' },
      {
        id: 'pick',
        type: 'card',
        title: 'Fast lane',
        text: 'Runs first',
        meta: '2 jobs',
        tone: 'ok',
        is_selectable: true,
      },
    ],
  });

const status = (id: string, title: string, extension: string) =>
  view({
    id,
    title,
    extension,
    placement: 'panel',
    nodes: [{ id: 's', type: 'status', text: `${title} is waiting`, tone: 'running' }],
  });

describe('live view placements', () => {
  it('reads the placement and the owner, and a view without one is inline', () => {
    const panel = form();
    expect(livePlacement(panel)).toBe('panel');
    expect(panel.extension).toBe('deploy');
    expect(livePlacement(view({ id: 'x', nodes: [{ id: 'p', type: 'paragraph', text: 'Hi' }] }))).toBe('inline');
    expect(liveInputNodes(panel.nodes).map((node) => node.id)).toEqual(['name', 'token']);
  });
});

describe('live inputs and cards', () => {
  it('sends a change after a pause in the typing, and at once on blur', () => {
    vi.useFakeTimers();
    const onChange = vi.fn();
    render(<LiveViewPanel view={form()} onChange={onChange} inDialog />);
    const name = screen.getByLabelText('Name');
    expect(name).toHaveValue('Ada');
    fireEvent.change(name, { target: { value: 'Ada L' } });
    fireEvent.change(name, { target: { value: 'Ada Lovelace' } });
    expect(onChange).not.toHaveBeenCalled();
    act(() => vi.advanceTimersByTime(LIVE_CHANGE_DEBOUNCE_MS));
    expect(onChange).toHaveBeenCalledTimes(1);
    expect(onChange).toHaveBeenLastCalledWith('name', 'Ada Lovelace');
    fireEvent.change(name, { target: { value: 'Grace' } });
    fireEvent.blur(name);
    expect(onChange).toHaveBeenCalledTimes(2);
    expect(onChange).toHaveBeenLastCalledWith('name', 'Grace');
  });

  it('submits every input, but leaves a secret the host already holds', async () => {
    const onChange = vi.fn();
    const onSubmit = vi.fn();
    render(<LiveViewPanel view={form()} onChange={onChange} onSubmit={onSubmit} inDialog />);
    fireEvent.change(screen.getByLabelText('Name'), { target: { value: 'Linus' } });
    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: 'Start' }));
    });
    expect(onChange).toHaveBeenCalledWith('name', 'Linus');
    expect(onSubmit).toHaveBeenCalledWith('go', { name: 'Linus' });
  });

  // User report 2026-11-14: a row of buttons that are tabs did not show the open tab.
  it('shows which button of a choice holds now', async () => {
    const tabs = view({
      id: 'tabs',
      nodes: [
        { id: 'run', type: 'button', label: 'Run', is_selected: true },
        { id: 'history', type: 'button', label: 'History', is_selected: false },
        { id: 'go', type: 'button', label: 'Start' },
      ],
    });
    const onActivate = vi.fn(async () => {});
    render(<LiveViewPanel view={tabs} onActivate={onActivate} inDialog />);
    expect(screen.getByRole('button', { name: 'Run' })).toHaveAttribute('aria-pressed', 'true');
    expect(screen.getByRole('button', { name: 'History' })).toHaveAttribute('aria-pressed', 'false');
    expect(screen.getByRole('button', { name: 'Start' })).not.toHaveAttribute('aria-pressed');
    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: 'History' }));
    });
    expect(onActivate).toHaveBeenCalledWith('history');
  });
});

const client = () =>
  ({
    viewAction: vi.fn(async () => ({ action: 'activate', view_id: 'v', is_accepted: true })),
    liveViewLog: vi.fn(),
  }) as unknown as GatewayClient;

describe('the side panel', () => {
  // User report 2026-11-09: the review sidebar opened 384px wide, so the T3 Code style
  // toolbar hid its menus; and the first width was stored, so no new default applied.
  it('opens at 42% of the window within 360 to 560 px, and keeps only a width that the person chose', () => {
    expect([800, 1280, 1600].map(initialPanelWidth)).toEqual([360, 538, 560]);
    window.localStorage.clear();
    vi.stubGlobal('innerWidth', 1280);
    try {
      render(<ResizablePanel>body</ResizablePanel>);
      const edge = screen.getByRole('separator', { name: 'Resize live views' });
      expect(edge.getAttribute('aria-valuenow')).toBe('538');
      expect(window.localStorage.length).toBe(0);
      fireEvent.keyDown(edge, { key: 'ArrowLeft' });
      expect(edge.getAttribute('aria-valuenow')).toBe('554');
      expect(window.localStorage.getItem('vis.live-panel.chosen-width')).toBe('554');
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it('shows one tab for each owner when views come from two extensions', () => {
    const views = [status('a', 'Deploy board', 'deploy'), status('b', 'Job queue', 'jobs')];
    render(<LivePanel views={views} client={client()} sid="s1" onClose={() => {}} />);
    const tabs = screen.getAllByRole('tab');
    expect(tabs.map((tab) => tab.textContent)).toEqual(['deploy', 'jobs']);
    expect(screen.getByText('Deploy board')).toBeVisible();
    expect(screen.queryByText('Job queue')).toBeNull();
    fireEvent.click(tabs[1]);
    expect(screen.getByText('Job queue')).toBeVisible();
    expect(screen.queryByText('Deploy board')).toBeNull();
  });

  // User report 2026-11-14: the tool panel of an extension offered `Interrupt`, which
  // stops a run. A panel is no run: it stays while the session is open.
  it('offers no interrupt on a panel view, and keeps it on a run', () => {
    render(<LivePanel views={[status('a', 'Deploy board', 'deploy')]} client={client()} sid="s1" onClose={() => {}} />);
    expect(screen.getByText('Deploy board is waiting')).toBeVisible();
    expect(screen.queryByRole('button', { name: 'Interrupt' })).toBeNull();
    cleanup();
    const run = view({ id: 'r', nodes: [{ id: 's', type: 'status', text: 'Deploying', tone: 'running' }] });
    render(<LiveViews views={[run]} client={client()} sid="s1" />);
    expect(screen.getByRole('button', { name: 'Interrupt' })).toBeVisible();
  });
});
