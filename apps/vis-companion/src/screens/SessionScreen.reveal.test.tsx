// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from 'vitest';
import { act, screen, waitFor } from '@testing-library/react';

import { renderSessionScreen } from './session-screen-harness';

// Regression, session 617d3b77-8522-4866-b4b4-01cc8253bf1a: an image-heavy traced
// answer stayed hidden behind "Loading session…" while the off-screen history
// ramped in — the screen waited for the WHOLE visible window to hydrate instead
// of the first painted turns.
//
// The scroll ARITHMETIC this screen performs on its transcript — following the
// end, restoring a parked reader, telling its own correction from a gesture —
// lives in `lib/reading-position.ts` and is pinned there against real figures.
// jsdom lays nothing out (`scrollHeight` is 0), so a mounted screen can prove
// what it SHOWS, never how far it scrolled.
// Regression, session 976f705e-fd80-4787-adc6-1ae8388fdaa2: returning to a cached
// session still covered ready-to-paint rows with the cold-load sheet on every visit.
describe('returning to a cached session', () => {
  it('paints the cached transcript without showing the loading sheet', () => {
    renderSessionScreen({
      client: {
        cachedTranscript: () => [
          {
            turn_id: 'cached-turn',
            request: 'Already in memory',
            status: 'completed',
            iterations: [],
          },
        ],
      },
    });

    expect(screen.getByText('Already in memory')).toBeInTheDocument();
    expect(screen.queryByLabelText('Loading recent turns')).not.toBeInTheDocument();
  });

  it('treats a cached empty transcript as ready rather than cold', () => {
    renderSessionScreen({ client: { cachedTranscript: () => [] } });

    expect(screen.queryByLabelText('Loading recent turns')).not.toBeInTheDocument();
  });
});

describe('opening a session', () => {
  it('shows the transcript instead of the loading sheet once turns arrive', async () => {
    renderSessionScreen({
      client: {
        transcript: () =>
          Promise.resolve([
            {
              turn_id: 't1',
              request: 'Rename the machine tag',
              status: 'completed',
              iterations: [],
            },
          ]),
      },
    });

    expect(screen.getByLabelText('Loading recent turns')).toBeInTheDocument();
    expect(await screen.findByText('Rename the machine tag')).toBeInTheDocument();
    await waitFor(() =>
      expect(screen.queryByLabelText('Loading recent turns')).not.toBeInTheDocument(),
    );
  });

  // Regression, user report: the opaque opening sheet only said "Loading session",
  // hid already-arrived turns without saying how many were still being prepared, then
  // faded through a briefly empty-looking frame while the scroll position caught up.
  it('reports opening progress and reveals the placed transcript atomically', async () => {
    let resolveTranscript!: (
      turns: Array<{
        turn_id: string;
        request: string;
        status: string;
        iterations: never[];
      }>,
    ) => void;
    const transcript = new Promise<Parameters<typeof resolveTranscript>[0]>((resolve) => {
      resolveTranscript = resolve;
    });
    renderSessionScreen({ client: { transcript: () => transcript } });

    const loading = screen.getByLabelText('Loading recent turns');
    expect(loading).toHaveTextContent('Loading recent turns…');
    expect(loading.parentElement).not.toHaveClass('transition-opacity', 'duration-200');

    // A slow network response must not advance a counter for turns that do not exist yet.
    await new Promise((resolve) => window.setTimeout(resolve, 50));

    resolveTranscript(
      Array.from({ length: 8 }, (_, index) => ({
        turn_id: `t${index + 1}`,
        request: `Turn ${index + 1}`,
        status: 'completed',
        iterations: [],
      })),
    );

    expect(await screen.findByText('Preparing 2 of 8 recent turns…')).toBeInTheDocument();
  });

  // Regression, Blockether/vis#316: the gutter of the outline rail came only when the veil
  // dropped. Every line of the transcript then wrapped again at a new width in the first
  // frames after the reveal, and the reader saw the text jump.
  it('keeps the outline gutter under the veil, so the reveal keeps the text width', async () => {
    renderSessionScreen({
      client: {
        // The real client answers its window at once; the harness default is a promise.
        transcriptWindow: () => ({ offset: 0, total: 8 }),
        transcript: () =>
          Promise.resolve(
            Array.from({ length: 8 }, (_, index) => ({
              turn_id: `t${index + 1}`,
              request: `Turn ${index + 1}`,
              status: 'completed',
              iterations: [],
            })),
          ),
      },
    });

    expect(await screen.findByText(/Preparing \d of 8 recent turns…/)).toBeInTheDocument();
    const column = document.querySelector('.transcript-column');
    expect(column).toHaveClass('has-outline');
    expect(screen.queryByRole('button', { name: 'Jump to a message' })).not.toBeInTheDocument();

    expect(await screen.findByRole('button', { name: 'Jump to a message' })).toBeInTheDocument();
    expect(column).toHaveClass('has-outline');
  });

  // Regression, Vis session 77fc84b5-5d0a-4780-ae96-b7f5e3f78b46: switching
  // among several live sessions while one transcript read was still in flight let
  // that old response repaint the new session, and left each abandoned request
  // occupying the transport that a later New session request needed.
  it('discards a transcript response from the session already left', async () => {
    type Turn = {
      turn_id: string;
      request: string;
      status: string;
      iterations: never[];
    };
    const resolvers = new Map<string, (turns: Turn[]) => void>();
    const signals = new Map<string, AbortSignal | undefined>();
    const view = renderSessionScreen({
      session: {
        id: 'first',
        title: 'First',
        status: 'idle',
        live: false,
        current_turn_id: null,
        turn_count: 0,
        server_time_ms: 0,
      },
      client: {
        cachedSession: (sid: string) => ({
          id: sid,
          title: sid,
          status: 'idle',
          live: false,
          current_turn_id: null,
          turn_count: 0,
          server_time_ms: 0,
        }),
        cachedTranscript: () => null,
        session: (sid: string) =>
          Promise.resolve({
            id: sid,
            title: sid,
            status: 'idle',
            live: false,
            current_turn_id: null,
            turn_count: 0,
            server_time_ms: 0,
          }),
        transcript: (sid: string, signal?: AbortSignal) => {
          signals.set(sid, signal);
          return new Promise<Turn[]>((resolve) => resolvers.set(sid, resolve));
        },
      },
    });

    await waitFor(() => expect(resolvers.has('first')).toBe(true));
    view.rerenderSession('second');
    await waitFor(() => expect(resolvers.has('second')).toBe(true));

    await act(async () => {
      resolvers.get('second')?.([
        {
          turn_id: 'second-turn',
          request: 'The current session',
          status: 'completed',
          iterations: [],
        },
      ]);
    });
    expect(await screen.findByText('The current session')).toBeInTheDocument();

    await act(async () => {
      resolvers.get('first')?.([
        {
          turn_id: 'first-turn',
          request: 'The session already left',
          status: 'completed',
          iterations: [],
        },
      ]);
    });

    expect(screen.queryByText('The session already left')).not.toBeInTheDocument();
    expect(screen.getByText('The current session')).toBeInTheDocument();
    expect(signals.get('first')?.aborted).toBe(true);
  });

  // Regression, user report: a response shorter than the transcript viewport stayed
  // against the header and left most of the phone as an empty band above the composer.
  // The transcript's minimum viewport height must give that spare height to its TOP,
  // keeping the newest response beside the composer where subsequent chunks arrive.
  it('bottom-aligns a short response instead of leaving a blank lower viewport', async () => {
    renderSessionScreen({
      client: {
        transcript: () =>
          Promise.resolve([
            {
              turn_id: 't-short',
              request: 'Give me the short answer',
              status: 'completed',
              iterations: [],
            },
          ]),
      },
    });

    expect(await screen.findByText('Give me the short answer')).toBeInTheDocument();
    const viewport = screen.getByRole('region', { name: 'Transcript' });
    expect(viewport.firstElementChild).toHaveClass('flex', 'flex-col', 'justify-end');
  });
});

/**
 * jsdom lays nothing out. Place the transcript view, and the mark of a long turn's earlier
 * steps. The mark reports itself near at once, as a real observer does for a mark in view.
 */
function placeEarlierSteps(markTop: number) {
  const measure = Element.prototype.getBoundingClientRect;
  vi.spyOn(Element.prototype, 'getBoundingClientRect').mockImplementation(function (this: Element) {
    let top: number;
    if (this.hasAttribute('data-earlier-steps')) top = markTop;
    else if (this.hasAttribute('data-keeps-reading-position')) top = 0;
    else return measure.call(this);
    return { top, bottom: top + 800, left: 0, right: 400, width: 400, height: 800, x: 0, y: top } as DOMRect;
  });
  vi.stubGlobal('IntersectionObserver', class {
    private readonly notify: IntersectionObserverCallback;
    constructor(notify: IntersectionObserverCallback) { this.notify = notify; }
    observe(target: Element) {
      if (!target.hasAttribute('data-earlier-steps')) return;
      this.notify([{ target, isIntersecting: true } as IntersectionObserverEntry], this as never);
    }
    unobserve() {}
    disconnect() {}
    takeRecords() { return []; }
  });
}

const latest = { id: 'i9', position: 9, assistant_prose: 'Latest progress' };
const longTurn = {
  turn_id: 't-long',
  request: 'Make it work end to end',
  status: 'completed',
  iterations: [latest],
  iterations_offset: 8,
  iterations_total: 9,
};

// Regression, user report: a session opened, and a moment later a long turn in view
// grew when its earlier steps landed, so the view jumped. The sheet now covers that read.
describe('opening on a long turn in view', () => {
  afterEach(() => {
    vi.restoreAllMocks();
    vi.unstubAllGlobals();
  });

  it('holds the sheet until the earlier steps land', async () => {
    let land!: (rows: object[]) => void;
    const turnTrace = vi.fn(() => new Promise<object[]>((done) => { land = done; }));
    placeEarlierSteps(300);
    renderSessionScreen({ client: { cachedTranscript: () => [longTurn], turnTrace } });

    expect(screen.getByLabelText('Loading recent turns')).toBeInTheDocument();
    expect(turnTrace).toHaveBeenCalledOnce();
    // Longer than the quiet frames that reveal a settled transcript.
    await act(() => new Promise((done) => setTimeout(done, 120)));
    expect(screen.getByLabelText('Loading recent turns')).toBeInTheDocument();
    await act(async () => land([{ id: 'i1', position: 1, assistant_prose: 'Earlier progress' }, latest]));
    await waitFor(() =>
      expect(screen.queryByLabelText('Loading recent turns')).not.toBeInTheDocument(),
    );
    expect(screen.getByText('Earlier progress')).toBeInTheDocument();
  });

  it('paints at once when the long turn starts above the view', () => {
    placeEarlierSteps(-40);
    renderSessionScreen({
      client: { cachedTranscript: () => [longTurn], turnTrace: () => new Promise(() => {}) },
    });

    expect(screen.getByText('Make it work end to end')).toBeInTheDocument();
    expect(screen.queryByLabelText('Loading recent turns')).not.toBeInTheDocument();
  });

  it('reveals the session when the earlier steps never land', async () => {
    placeEarlierSteps(300);
    renderSessionScreen({
      client: { cachedTranscript: () => [longTurn], turnTrace: () => new Promise(() => {}) },
    });

    expect(screen.getByLabelText('Loading recent turns')).toBeInTheDocument();
    await waitFor(
      () => expect(screen.queryByLabelText('Loading recent turns')).not.toBeInTheDocument(),
      { timeout: 2000 },
    );
  });
});
