// @vitest-environment jsdom
import { act, cleanup, fireEvent, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { renderSessionScreen, sessionFixture, subscriptionHub } from './session-screen-harness';
import type { Session, SseEvent } from '../lib/types';

afterEach(() => {
  cleanup();
  vi.useRealTimers();
});

function deferred<T>() {
  let resolve!: (value: T) => void;
  const promise = new Promise<T>((finish) => {
    resolve = finish;
  });
  return { promise, resolve };
}

async function advance(ms: number) {
  await act(async () => {
    await vi.advanceTimersByTimeAsync(ms);
  });
}

const waiting = {
  turnId: 'waiting',
  request: 'Next request',
  preview: 'Next request',
  attachments: [],
};
const paused = { reason: 'turn_failed', held: 1 };

describe('a queued turn after a failed request', () => {
  // Regression, Vis session 57dfea5e-0c2d-4190-a82c-0e1992e352c3: reopening the
  // session recovered the queued row but not queue.paused, so the app offered no
  // working way to continue that message after the provider recovered.
  it('recovers the paused marker with the backlog and continues it', async () => {
    const resumeQueue = vi.fn().mockResolvedValue(undefined);
    renderSessionScreen({
      client: {
        cachedQueuedTurns: () => [
          {
            turnId: 'waiting',
            request: 'Run this after recovery',
            preview: 'Run this after recovery',
            attachments: [],
          },
        ],
        cachedQueuePaused: () => ({ reason: 'turn_failed', held: 1 }),
        resumeQueue,
      },
    });

    expect(await screen.findByText('Run this after recovery')).toBeVisible();
    expect(screen.getByText('1 held · turn failed')).toBeVisible();

    fireEvent.click(screen.getByRole('button', { name: 'Continue queue' }));
    await waitFor(() => expect(resumeQueue).toHaveBeenCalledWith('s1'));
  });

  it('refreshes the queued snapshot with one inclusive session reconciliation request', async () => {
    vi.useFakeTimers();
    let cachedRows: typeof waiting[] = [];
    let cachedPause: typeof paused | null = null;
    let reads = 0;
    const readSession = vi.fn(async (_sid: string, _signal?: AbortSignal, includeQueued = false) => {
      reads += 1;
      if (reads > 1 && includeQueued) {
        cachedRows = [waiting];
        cachedPause = paused;
      }
      return sessionFixture();
    });
    const queuedTurns = vi.fn(async () => ({ turns: [waiting], paused }));
    await act(async () => {
      renderSessionScreen({
        client: {
          session: readSession,
          cachedQueuedTurns: () => cachedRows,
          cachedQueuePaused: () => cachedPause,
          queuedTurns,
        },
      });
    });
    expect(screen.queryByText('Queue paused')).toBeNull();

    await advance(5000);
    expect(readSession).toHaveBeenCalledTimes(2);
    expect(readSession).toHaveBeenLastCalledWith('s1', undefined, true);
    expect(queuedTurns).not.toHaveBeenCalled();
    expect(screen.getByText('Next request')).toBeVisible();
    expect(screen.getByText('1 held · turn failed')).toBeVisible();
  });

  it.each([
    ['session', 'queue.resumed'],
    ['session', 'queue.paused'],
    ['backlog', 'queue.resumed'],
    ['backlog', 'queue.paused'],
  ] as const)('keeps an older %s read from undoing %s', async (source, type) => {
    vi.useFakeTimers();
    const events = subscriptionHub();
    const staleRead = deferred<void>();
    const resumes = type === 'queue.resumed';
    const snapshotPause = resumes ? paused : null;
    let reads = 0;
    const readSession = vi.fn(async () => {
      reads += 1;
      if ((source === 'session' && reads === 1) || (source === 'backlog' && reads === 2)) {
        await staleRead.promise;
      }
      return sessionFixture();
    });
    const queuedTurns = vi.fn(async () => ({ turns: [waiting], paused: snapshotPause }));
    const resumeQueue = vi.fn().mockResolvedValue(undefined);
    await act(async () => {
      renderSessionScreen({
        client: {
          session: readSession,
          cachedQueuedTurns: () => [waiting],
          cachedQueuePaused: () => snapshotPause,
          queuedTurns,
          resumeQueue,
        },
        subscriptions: events,
      });
    });
    expect(readSession).toHaveBeenCalled();
    if (source === 'backlog') {
      await advance(5000);
      expect(readSession).toHaveBeenCalledTimes(2);
      expect(queuedTurns).not.toHaveBeenCalled();
    }

    if (resumes) {
      act(() =>
        events.emit({
          type: 'queue.paused',
          seq: 1,
          ...paused,
        } as unknown as SseEvent),
      );
      await advance(150);
      fireEvent.click(screen.getByRole('button', { name: 'Continue queue' }));
      expect(resumeQueue).toHaveBeenCalledWith('s1');
      expect(screen.getByText('Queue paused')).toBeInTheDocument();
    }
    act(() => events.emit({ type, seq: 2, ...paused } as unknown as SseEvent));
    await advance(150);
    expect(screen.queryByText('Queue paused') !== null).toBe(!resumes);

    await act(async () => {
      staleRead.resolve();
    });
    expect(screen.queryByText('Queue paused') !== null).toBe(!resumes);
    expect(screen.getByText('Next request')).toBeInTheDocument();

    // A later inclusive read still repairs a pause/resume whose live frame was missed.
    await advance(5000);
    expect(screen.queryByText('Queue paused') !== null).toBe(resumes);
  });

  it('does not carry a late paused snapshot into another session', async () => {
    const oldSession = deferred<Session>();
    const view = renderSessionScreen({
      client: {
        session: (sid: string) =>
          sid === 's1' ? oldSession.promise : Promise.resolve(sessionFixture({ id: sid })),
        cachedQueuedTurns: () => [waiting],
        cachedQueuePaused: (sid: string) => (sid === 's1' ? paused : null),
      },
    });
    await act(async () => {
      view.rerenderSession('s2');
    });
    expect(screen.queryByText('Queue paused')).toBeNull();

    await act(async () => {
      oldSession.resolve(sessionFixture());
    });
    expect(screen.queryByText('Queue paused')).toBeNull();
    expect(screen.getByText('Next request')).toBeInTheDocument();
  });
});
