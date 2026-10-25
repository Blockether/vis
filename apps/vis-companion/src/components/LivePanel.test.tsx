// @vitest-environment jsdom
// Live views outside the transcript: inputs and cards, the side panel and header pins.
import { act, cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { LIVE_CHANGE_DEBOUNCE_MS, LiveViewPanel } from './LiveView';
import { HeaderPins, LivePanel, LiveViewList } from './LivePanel';
import { useHeaderViews } from '../lib/header-views';
import type { GatewayClient } from '../lib/gateway';
import { liveInputNodes, liveViewFromWire, livePlacement, type LiveView } from '../lib/live-view';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import type { SseEvent } from '../lib/types';

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
    extension: 'swarm',
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

const status = (id: string, title: string, extension: string, placement = 'panel') =>
  view({
    id,
    title,
    extension,
    placement,
    nodes: [{ id: 's', type: 'status', text: `${title} is waiting`, tone: 'running' }],
  });

describe('live view placements', () => {
  it('reads the placement and the owner, and a view without one is inline', () => {
    const panel = form();
    expect(livePlacement(panel)).toBe('panel');
    expect(panel.extension).toBe('swarm');
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

  it('sends one click for a selectable card', async () => {
    const onActivate = vi.fn();
    render(<LiveViewPanel view={form()} onActivate={onActivate} inDialog />);
    const card = screen.getByRole('button', { name: /Fast lane/ });
    expect(within(card).getByText('2 jobs')).toBeVisible();
    await act(async () => {
      fireEvent.click(card);
    });
    expect(onActivate).toHaveBeenCalledWith('pick');
  });
});

const client = (calls: unknown[] = []) =>
  ({
    viewAction: vi.fn(async (...args: unknown[]) => {
      calls.push(['session', ...args]);
      return { action: 'activate', view_id: 'v', is_accepted: true };
    }),
    headerViewAction: vi.fn(async (...args: unknown[]) => {
      calls.push(['machine', ...args]);
      return { action: 'activate', view_id: 'v', is_accepted: true };
    }),
    headerViews: vi.fn(async () => [status('h1', 'Deploys', 'swarm', 'header')]),
    liveViewLog: vi.fn(),
  }) as unknown as GatewayClient;

describe('the side panel', () => {
  it('folds a view to its fold row and opens it again', () => {
    render(<LiveViewList views={[status('a', 'Swarm board', 'swarm')]} client={client()} sid="s1" />);
    const row = screen.getByRole('button', { name: /Swarm board/ });
    expect(row).toHaveAttribute('aria-expanded', 'true');
    expect(screen.getByText('Swarm board is waiting')).toBeVisible();
    fireEvent.click(row);
    expect(row).toHaveAttribute('aria-expanded', 'false');
    expect(screen.getByText('Swarm board is waiting')).toBeVisible();
    fireEvent.click(row);
    expect(row).toHaveAttribute('aria-expanded', 'true');
  });

  it('shows one tab for each owner when views come from two extensions', () => {
    const views = [status('a', 'Swarm board', 'swarm'), status('b', 'Forge queue', 'forge')];
    render(<LivePanel views={views} client={client()} sid="s1" onClose={() => {}} />);
    const tabs = screen.getAllByRole('tab');
    expect(tabs.map((tab) => tab.textContent)).toEqual(['swarm', 'forge']);
    expect(screen.getByText('Swarm board')).toBeVisible();
    expect(screen.queryByText('Forge queue')).toBeNull();
    fireEvent.click(tabs[1]);
    expect(screen.getByText('Forge queue')).toBeVisible();
    expect(screen.queryByText('Swarm board')).toBeNull();
  });

  it('names the one owner and closes', () => {
    const onClose = vi.fn();
    render(<LivePanel views={[status('a', 'Swarm board', 'swarm')]} client={client()} sid="s1" onClose={onClose} />);
    expect(screen.queryByRole('tablist')).toBeNull();
    expect(screen.getByRole('heading', { name: 'swarm' })).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: 'Close live views' }));
    expect(onClose).toHaveBeenCalled();
  });
});

describe('header pins', () => {
  it('shows four pins, then +N, and sends actions on the machine route', async () => {
    const calls: unknown[] = [];
    const pins = [1, 2, 3, 4, 5, 6].map((n) => status(`h${n}`, `Deploy ${n}`, 'swarm', 'header'));
    const withButton = {
      ...pins[0],
      nodes: [...pins[0].nodes, { id: 'go', type: 'button' as const, label: 'Retry', action: 'activate' as const, is_disabled: false, clicks: 0 }],
    };
    render(<HeaderPins views={[withButton, ...pins.slice(1)]} client={client(calls)} />);
    const list = screen.getByRole('list', { name: 'Pinned views' });
    expect(within(list).getAllByRole('listitem')).toHaveLength(5);
    expect(screen.getByRole('button', { name: 'Show all 6 pinned views' })).toHaveTextContent('+2');
    fireEvent.click(screen.getByRole('button', { name: /^Deploy 1/ }));
    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: 'Retry' }));
    });
    expect(calls).toEqual([['machine', 'h1', { action: 'activate', node_id: 'go' }]]);
  });

  it('keeps header views current from the fleet stream', async () => {
    let listener: ((event: SseEvent) => void) | null = null;
    const hub = {
      subscribeFleet: (next: (event: SseEvent) => void) => {
        listener = next;
        return () => {};
      },
      subscribeFleetState: (next: (live: boolean) => void) => {
        next(true);
        return () => {};
      },
    } as unknown as SessionSubscriptionHub;
    const gateway = client();
    function Probe() {
      const views = useHeaderViews(gateway, hub);
      return <p>{views.map((item) => item.title).join(',')}</p>;
    }
    render(<Probe />);
    expect(await screen.findByText('Deploys')).toBeVisible();
    const open = (id: string, title: string, placement: string): SseEvent =>
      ({
        type: 'view.open',
        kind: 'live',
        view_id: id,
        view: { id, title, placement, seq: 0, nodes: [{ id: 'p', type: 'paragraph', text: 'x' }] },
      }) as unknown as SseEvent;
    act(() => listener!(open('h2', 'Builds', 'header')));
    act(() => listener!(open('p1', 'Board', 'panel')));
    expect(screen.getByText('Deploys,Builds')).toBeVisible();
    act(() => listener!({ type: 'view.close', kind: 'live', view_id: 'h1' } as unknown as SseEvent));
    expect(screen.getByText('Builds')).toBeVisible();
  });
});
