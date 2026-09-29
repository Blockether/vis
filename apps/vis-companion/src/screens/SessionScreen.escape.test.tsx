// @vitest-environment jsdom
import { useEffect, useState } from 'react';
import { flushSync } from 'react-dom';
import { describe, expect, it, vi } from 'vitest';
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';

import { SessionSearchDialog } from '../components/SessionSearchDialog';
import { isLayerUp, useBackLayer } from '../lib/edge-back';
import { renderSessionScreen } from './session-screen-harness';

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
    const field = screen.getByRole('searchbox', { name: 'Search sessions on every machine' });
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
});
