// @vitest-environment jsdom
// Live views outside the transcript: inputs and cards, and the side panel.
import { act, cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { LIVE_CHANGE_DEBOUNCE_MS, LiveViewPanel } from './LiveView';
import { LivePanel } from './LivePanel';
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
});

const client = () =>
  ({
    viewAction: vi.fn(async () => ({ action: 'activate', view_id: 'v', is_accepted: true })),
    liveViewLog: vi.fn(),
  }) as unknown as GatewayClient;

describe('the side panel', () => {
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
});
