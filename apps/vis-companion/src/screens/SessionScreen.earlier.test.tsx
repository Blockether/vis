// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { act, fireEvent, screen, waitFor } from '@testing-library/react';

import { renderSessionScreen } from './session-screen-harness';

type Watcher = { notify: IntersectionObserverCallback; targets: Set<Element> };
const watchers = new Set<Watcher>();

/** jsdom has no layout. The test says when the mark above the oldest turn comes near. */
function scrollNearOldest() {
  for (const watcher of [...watchers]) {
    for (const target of watcher.targets) {
      if (!target.hasAttribute('data-earlier-turns')) continue;
      watcher.notify([{ target, isIntersecting: true } as IntersectionObserverEntry], {} as never);
    }
  }
}

const row = (index: number) => ({
  turn_id: `t${index}`,
  request: `Turn ${index}`,
  status: 'completed',
  iterations: [],
});
const rows = (from: number, to: number) => Array.from({ length: to - from }, (_, at) => row(from + at));

/** A session of `total` turns that holds the newest `held` of them. */
function sessionHolding(
  held: number,
  total: number,
  transcriptEarlier: () => Promise<unknown>,
  warmTurnTraces: () => Promise<void> | null = () => null,
) {
  let offset = total - held;
  return renderSessionScreen({
    client: {
      cachedTranscript: () => rows(offset, total),
      transcriptWindow: () => ({ offset, total }),
      transcriptEarlier: () =>
        transcriptEarlier().then((older) => {
          offset = 0;
          return older;
        }),
      warmTurnTraces,
    },
  });
}

// User report: the transcript had a "Load earlier" button. Scrolling up now loads earlier turns.
describe('scrolling up to the oldest turn', () => {
  beforeEach(() => {
    vi.stubGlobal(
      'IntersectionObserver',
      class {
        private readonly watcher: Watcher;
        constructor(notify: IntersectionObserverCallback) {
          this.watcher = { notify, targets: new Set() };
          watchers.add(this.watcher);
        }
        observe(target: Element) {
          this.watcher.targets.add(target);
        }
        unobserve(target: Element) {
          this.watcher.targets.delete(target);
        }
        disconnect() {
          watchers.delete(this.watcher);
        }
        takeRecords() {
          return [];
        }
      },
    );
  });

  afterEach(() => {
    watchers.clear();
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
  });

  it('loads the earlier page without a click', async () => {
    const transcriptEarlier = vi.fn(() => Promise.resolve(rows(0, 30)));
    sessionHolding(6, 30, transcriptEarlier);
    await screen.findByText('Turn 24');

    await waitFor(() => {
      scrollNearOldest();
      expect(transcriptEarlier).toHaveBeenCalledOnce();
    });
    // The page joins the held turns. The mark, still near, then reveals them.
    await waitFor(() => {
      scrollNearOldest();
      expect(screen.getByText('Turn 23')).toBeInTheDocument();
    });
  });

  // User report: the transcript jumped some turns back in the first flick of a session.
  // The session opened with fewer turns than its window takes, and the page that landed
  // in the flick mounted into the spare room, above the reader.
  it('holds a page that lands while the window has spare room', async () => {
    const transcriptEarlier = vi.fn(() => Promise.resolve(rows(0, 30)));
    sessionHolding(3, 30, transcriptEarlier);
    await screen.findByText('Turn 27');

    await waitFor(() => {
      scrollNearOldest();
      expect(transcriptEarlier).toHaveBeenCalledOnce();
    });
    await act(async () => {
      await transcriptEarlier.mock.results[0].value;
    });
    // Each turn of the page is still held above the three turns on screen.
    expect(screen.getByLabelText('27 earlier turns')).toBeInTheDocument();

    await waitFor(() => {
      scrollNearOldest();
      expect(screen.getByText('Turn 26')).toBeInTheDocument();
    });
  });

  // User report: a session full of images jumped under a reader who scrolled up. Its turns
  // mounted with their newest steps only, and the earlier steps made them taller later.
  it('mounts earlier turns after their earlier steps are read', async () => {
    let finishRead!: () => void;
    const read = new Promise<void>((resolve) => {
      finishRead = resolve;
    });
    sessionHolding(12, 12, () => Promise.resolve(null), () => read);
    // The mark arms when the opening window is mounted to its oldest turn.
    await waitFor(() =>
      expect(
        [...watchers].some((watcher) =>
          [...watcher.targets].some((target) => target.hasAttribute('data-earlier-turns')),
        ),
      ).toBe(true),
    );

    await act(async () => scrollNearOldest());
    // The window did not widen: the rule above the oldest turn still counts the held turns.
    expect(screen.getByLabelText('4 earlier turns')).toBeInTheDocument();
    expect(screen.queryByText('Turn 3')).not.toBeInTheDocument();

    await act(async () => finishRead());
    expect(await screen.findByText('Turn 3')).toBeInTheDocument();
  });

  it('reads the next page while held turns are still above the reader', async () => {
    const transcriptEarlier = vi.fn(() => new Promise<unknown>(() => {}));
    sessionHolding(12, 40, transcriptEarlier);
    await screen.findByText('Turn 39');
    expect(screen.queryByText('Turn 31')).not.toBeInTheDocument();

    await waitFor(() => {
      scrollNearOldest();
      expect(screen.getByText('Turn 31')).toBeInTheDocument();
    });
    expect(transcriptEarlier).toHaveBeenCalledOnce();
  });

  it('waits for the reader to retry a failed page', async () => {
    const transcriptEarlier = vi.fn(() => Promise.reject(new Error('gateway down')));
    sessionHolding(6, 30, transcriptEarlier);
    await screen.findByText('Turn 24');

    await waitFor(() => {
      scrollNearOldest();
      expect(transcriptEarlier).toHaveBeenCalledOnce();
    });
    const retry = await screen.findByRole('button', { name: 'Try loading earlier turns again' });
    await act(async () => scrollNearOldest());
    expect(transcriptEarlier).toHaveBeenCalledOnce();

    fireEvent.click(retry);
    await waitFor(() => {
      scrollNearOldest();
      expect(transcriptEarlier).toHaveBeenCalledTimes(2);
    });
  });
});
