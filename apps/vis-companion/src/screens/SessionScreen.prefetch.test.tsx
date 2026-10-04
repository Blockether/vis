// @vitest-environment jsdom
import { act, screen, waitFor } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { GatewayClient } from '../lib/gateway';
import { renderSessionScreen, sessionFixture } from './session-screen-harness';

beforeEach(() => { localStorage.clear(); });

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

const first = {
  turn_id: 'turn-1',
  request: 'The previous question',
  status: 'completed',
  iterations: [],
  content: [{ id: 'answer-1', type: 'prose', markdown: 'The previous answer' }],
};
const latest = {
  turn_id: 'turn-2',
  request: 'The new question',
  status: 'completed',
  iterations: [],
  content: [{ id: 'answer-2', type: 'prose', markdown: 'The new answer' }],
};
const body = (turns: typeof first[]) => new Response(JSON.stringify({
  turns, total: turns.length, offset: 0, has_more: false,
}));

function screenClient(client: GatewayClient, row: ReturnType<typeof sessionFixture>) {
  return {
    base: client.base,
    cachedSession: client.cachedSession.bind(client),
    cachedTranscript: client.cachedTranscript.bind(client),
    transcriptWindow: client.transcriptWindow.bind(client),
    openingTranscript: client.openingTranscript.bind(client),
    transcriptIfMoved: client.transcriptIfMoved.bind(client),
    session: async () => row,
  };
}

// Regression: a joined prefetch returned "unchanged", but the screen still held older rows.
describe('opening a prefetched answer', () => {
  it.each(['before', 'during'] as const)(
    'adopts a warm that finishes %s revalidation',
    async (timing) => {
      const client = new GatewayClient({ url: `http://gateway.example.com/prefetch-${timing}` });
      const previous = sessionFixture({ live: false, turn_count: 1, modified_at: 'before' });
      const next = { ...previous, turn_count: 2, modified_at: 'after' };
      let release!: (response: Response) => void;
      const pending = new Promise<Response>((resolve) => { release = resolve; });
      const fetched = vi.fn().mockResolvedValueOnce(body([first])).mockReturnValue(pending);
      vi.stubGlobal('fetch', fetched);
      await client.transcriptIfMoved('s1', previous);
      client.warmTranscript(next);
      let confirm!: (row: typeof next) => void;
      const metadata = new Promise<typeof next>((resolve) => { confirm = resolve; });
      renderSessionScreen({
        session: next,
        client: {
          ...screenClient(client, next),
          session: () => timing === 'before' ? metadata : Promise.resolve(next),
        },
      });
      expect(screen.getByText('The previous answer')).toBeInTheDocument();
      await waitFor(() => expect(fetched).toHaveBeenCalledTimes(2));

      await act(async () => { release(body([first, latest])); });
      await waitFor(() => expect(client.cachedTranscript('s1')).toHaveLength(2));
      await act(async () => { confirm(next); });
      expect(await screen.findByText('The new answer')).toBeInTheDocument();
      expect(fetched).toHaveBeenCalledTimes(2);
    },
  );

  it('paints NEW from the first frame without waiting for session revalidation', async () => {
    const row = sessionFixture({
      live: false, turn_count: 1, modified_at: 'ready', is_unread: true, unread_answers: 1,
    });
    const client = new GatewayClient({ url: 'http://gateway.example.com/prefetch-ready' });
    const fetched = vi.fn(async (input: string) =>
      new URL(String(input)).pathname.endsWith('/transcript')
        ? body([latest]) : new Response(JSON.stringify({ sessions: [row], total: 1 })),
    );
    vi.stubGlobal('fetch', fetched);
    await client.listSessions();
    // The list publishes first and warms the NEW answer behind it.
    await vi.waitFor(() => expect(client.cachedTranscript(row.id)?.map((turn) => turn.turn_id)).toEqual(['turn-2']));
    renderSessionScreen({
      session: row,
      client: {
        ...screenClient(client, row),
        session: () => new Promise(() => {}),
      },
    });
    expect(screen.getByText('The new answer')).toBeInTheDocument();
    expect(screen.queryByLabelText('Loading recent turns')).not.toBeInTheDocument();
    expect(fetched).toHaveBeenCalledTimes(2);
  });
});
