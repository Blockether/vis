// @vitest-environment jsdom
import { useEffect, useState } from 'react';
import { flushSync } from 'react-dom';
import { describe, expect, it, vi } from 'vitest';
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';

import { SessionSearchDialog } from '../components/SessionSearchDialog';
import { isLayerUp, useBackLayer } from '../lib/edge-back';
import { readZenMode, setZenMode } from '../lib/transcript-display';
import { renderSessionScreen, sessionFixture } from './session-screen-harness';

const runningTurn = () => ({
  turn: {
    id: 'turn-1',
    request: 'Check the logs',
    answer: '',
    iterations: [],
    startedAt: Date.now(),
    status: 'running',
  },
  seq: 1,
});

function renderRunning() {
  const cancelTurn = vi.fn(() => Promise.resolve(null));
  renderSessionScreen({ client: { cachedRunningTurn: runningTurn, cancelTurn } });
  return cancelTurn;
}

/** An opened run: a layer that closes on Escape without claiming the key. */
function OpenedRun({ onEscape }: { onEscape: () => void }) {
  useBackLayer(() => {});
  useEffect(() => {
    const onKey = (event: KeyboardEvent) => {
      if (event.key === 'Escape') onEscape();
    };
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [onEscape]);
  return <p>Opened run</p>;
}

function SelfClosingRun({ onGone }: { onGone: (isLayerStillUp: boolean) => void }) {
  const [isOpen, setIsOpen] = useState(true);
  return isOpen ? (
    <OpenedRun
      onEscape={() => {
        flushSync(() => setIsOpen(false));
        onGone(isLayerUp());
      }}
    />
  ) : null;
}

// Reported: Escape in the session search, opened while a turn ran, stopped the turn and
// left the dialog open. Escape stops the turn only when nothing above the screen wants it.
describe('Escape over a running turn', () => {
  it('stops the turn when nothing stands over the screen', async () => {
    const cancelTurn = renderRunning();
    fireEvent.keyDown(document.body, { key: 'Escape' });
    await waitFor(() => expect(cancelTurn).toHaveBeenCalledWith('s1', 'turn-1'));
  });

  it('closes the session search and leaves the turn running', async () => {
    const cancelTurn = renderRunning();
    const onClose = vi.fn();
    render(
      <SessionSearchDialog query="" onQuery={() => {}} onClose={onClose} results={<p>No recent sessions</p>} />,
    );
    const field = screen.getByRole('searchbox', { name: 'Search session titles and messages' });
    expect(field).toHaveFocus();

    fireEvent.keyDown(field, { key: 'Escape' });
    await act(async () => {});
    expect(onClose).toHaveBeenCalledTimes(1);
    expect(cancelTurn).not.toHaveBeenCalled();
  });

  it('leaves the turn running when a layer takes itself down on the same key', async () => {
    // The layer listens BEFORE the screen does, so it is gone by the time the screen hears
    // the key; what the screen reads is the layer that stood there as the key arrived.
    const gone = vi.fn();
    render(<SelfClosingRun onGone={gone} />);
    const cancelTurn = renderRunning();

    fireEvent.keyDown(document.body, { key: 'Escape' });
    await act(async () => {});
    expect(gone).toHaveBeenCalledWith(false);
    expect(screen.queryByText('Opened run')).not.toBeInTheDocument();
    expect(cancelTurn).not.toHaveBeenCalled();
  });

  it('leaves the turn running under a modal surface that is not a registered layer', async () => {
    const cancelTurn = renderRunning();
    render(<div role="dialog" aria-modal="true" aria-label="Picture" />);

    fireEvent.keyDown(document.body, { key: 'Escape' });
    await act(async () => {});
    expect(cancelTurn).not.toHaveBeenCalled();
  });

  it('leaves the turn running when a control already answered the key', async () => {
    const cancelTurn = renderRunning();
    render(<input aria-label="Picker" onKeyDown={(event) => event.preventDefault()} />);

    fireEvent.keyDown(screen.getByRole('textbox', { name: 'Picker' }), { key: 'Escape' });
    await act(async () => {});
    expect(cancelTurn).not.toHaveBeenCalled();
  });
  // User request 2026-11-07: a person stops subagents only through the leader. The stop of the
  // leader turn also stops its subagents, so a subagent session has no stop of its own.
  it('has no stop in a working subagent session, and Escape leaves its turn running', async () => {
    const cancelTurn = vi.fn(() => Promise.resolve(null));
    const row = { session_id: 's1', parent_id: 'lead', leader_id: 'lead', team_id: 't1', task: 'Fix', status: 'running', iteration_budget: 4, iterations_used: 1 };
    renderSessionScreen({
      session: sessionFixture({ title: 'Fix the parser', agent: { role: 'subagent', parent_id: 'lead', leader_id: 'lead' } }),
      client: { cachedRunningTurn: runningTurn, cancelTurn, cachedAgents: () => [row] },
    });
    await screen.findByRole('heading', { name: /^leader →/ });
    expect(screen.queryByRole('button', { name: /stop/i })).toBeNull();

    const event = new KeyboardEvent('keydown', { key: 'Escape', bubbles: true, cancelable: true });
    act(() => {
      document.body.dispatchEvent(event);
    });
    await act(async () => {});
    expect(event.defaultPrevented).toBe(false);
    expect(cancelTurn).not.toHaveBeenCalled();
  });
});

// Reported: in Safari's full screen window, Escape in Zen mode left full screen and kept
// Zen. Safari leaves full screen on every Escape the page does not claim.
describe('Escape in Zen mode', () => {
  const escape = () => {
    const event = new KeyboardEvent('keydown', { key: 'Escape', bubbles: true, cancelable: true });
    act(() => {
      document.body.dispatchEvent(event);
    });
    return event;
  };

  it('leaves Zen mode and claims the key', () => {
    setZenMode(true);
    renderSessionScreen();

    expect(escape().defaultPrevented).toBe(true);
    expect(readZenMode()).toBe(false);
  });

  it('leaves Zen mode before it stops a running turn', async () => {
    setZenMode(true);
    const cancelTurn = renderRunning();

    escape();
    await act(async () => {});
    expect(readZenMode()).toBe(false);
    expect(cancelTurn).not.toHaveBeenCalled();
  });

  it('keeps Zen mode under a layer that owns the key', () => {
    setZenMode(true);
    renderSessionScreen();
    render(<div role="dialog" aria-modal="true" aria-label="Picture" />);

    escape();
    expect(readZenMode()).toBe(true);
    act(() => setZenMode(false));
  });
});

// Reported: after a stop, the queued rows stayed in the tray for seconds while their text
// was already back in the composer. The gateway drops them when it accepts the stop.
describe('Stop with a queued backlog', () => {
  it('moves the queued rows into the composer as soon as the stop is accepted', async () => {
    const cancelTurn = vi.fn(() => Promise.resolve(null));
    const queuedRow = {
      turnId: 'queued-1',
      request: 'Queued words',
      preview: 'Queued words',
      attachments: [],
    };
    renderSessionScreen({
      client: {
        cachedRunningTurn: runningTurn,
        cancelTurn,
        cachedQueuedTurns: () => [queuedRow],
        queuedTurns: () => Promise.resolve({ turns: [queuedRow], paused: null }),
      },
    });
    expect(await screen.findByRole('region', { name: 'Queued messages' })).toBeInTheDocument();

    fireEvent.keyDown(document.body, { key: 'Escape' });

    await waitFor(() => expect(cancelTurn).toHaveBeenCalledWith('s1', 'turn-1'));
    await waitFor(() =>
      expect(screen.queryByRole('region', { name: 'Queued messages' })).not.toBeInTheDocument(),
    );
    expect((screen.getByLabelText('Message Vis') as HTMLTextAreaElement).value).toBe('Queued words');
  });
});
