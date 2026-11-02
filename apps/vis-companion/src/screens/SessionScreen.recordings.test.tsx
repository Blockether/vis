// @vitest-environment jsdom
import { act, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { GatewayError } from '../lib/gateway';
import { renderSessionScreen } from './session-screen-harness';

// Regression: session 388aa964-07cf-498a-a6d7-71c49c86b491 reopened with a
// playable recording but no transcription, even after the gateway finished it.
describe('persisted recording transcriptions', () => {
  it.each([undefined, 'pending'])('refreshes a completed turn with status %s', async (status) => {
    const attachment = {
      id: 'recording',
      filename: 'meeting.m4a',
      media_type: 'audio/mp4',
      base64: 'AAAAIGZ0eXBNNEEg',
      transcription_status: status,
    };
    const turn = {
      turn_id: 'recorded-turn',
      request: 'Summarize the meeting',
      status: 'completed',
      iterations: [],
      attachments: [attachment],
    };
    const fetchTurnAttachments = vi
      .fn()
      .mockResolvedValueOnce([{ ...attachment, transcription_status: 'pending' }])
      .mockResolvedValue([{ ...attachment, transcription: 'We agreed to meet on Friday.' }]);
    const view = renderSessionScreen({
      client: {
        cachedTranscript: () => [turn],
        transcript: async () => [turn],
        fetchTurnAttachments,
      },
    });
    const player = view.container.querySelector('audio');
    await screen.findByText('TRANSCRIBING…');
    const disclosure = await screen.findByText('TRANSCRIPTION', {}, { timeout: 2500 });
    act(() => disclosure.click());
    expect(screen.getByText(/We agreed to meet on Friday/)).toBeInTheDocument();
    expect(view.container.querySelector('audio')).toBe(player);
    expect(fetchTurnAttachments).toHaveBeenCalledWith(
      's1',
      'recorded-turn',
      expect.any(AbortSignal),
      true,
    );
    await waitFor(() => expect(fetchTurnAttachments).toHaveBeenCalledTimes(2));
  });

  it('does not retry a settled transcription failure on every render', async () => {
    const turn = {
      turn_id: 'unavailable-turn',
      request: 'Listen',
      status: 'completed',
      iterations: [],
      attachments: [
        {
          filename: 'memo.wav',
          media_type: 'audio/wav',
          base64: 'AAAA',
          transcription_status: 'unavailable',
        },
      ],
    };
    const fetchTurnAttachments = vi.fn();
    renderSessionScreen({
      client: { transcript: async () => [turn], fetchTurnAttachments },
    });
    await screen.findByText('NO TRANSCRIPTION');
    expect(fetchTurnAttachments).not.toHaveBeenCalled();
  });
});

// Audit of Companion polls: a pending recording was read every second, forever.
// A deleted turn (404), a failed read or a lost worker kept the loop alive, also
// while the app was hidden, and each read can start transcription work.
describe('pending recording reads', () => {
  afterEach(() => {
    vi.useRealTimers();
    vi.restoreAllMocks();
  });

  const pendingTurn = {
    turn_id: 'recorded-turn',
    request: 'Summarize the meeting',
    status: 'completed',
    iterations: [],
    attachments: [
      { id: 'r', filename: 'm.m4a', media_type: 'audio/mp4', base64: 'AAAA', transcription_status: 'pending' },
    ],
  };

  async function readsInAMinute(fetchTurnAttachments: ReturnType<typeof vi.fn>) {
    vi.useFakeTimers();
    renderSessionScreen({
      client: {
        cachedTranscript: () => [pendingTurn],
        transcript: async () => [pendingTurn],
        fetchTurnAttachments,
      },
    });
    await act(() => vi.advanceTimersByTimeAsync(60_000));
    return fetchTurnAttachments.mock.calls.length;
  }

  it('backs off while the transcription stays pending', async () => {
    // 1 + reads after 1, 2, 4, 8, 16 and 30 s: 7 reads by 61 s instead of 60.
    const reads = await readsInAMinute(vi.fn().mockResolvedValue(pendingTurn.attachments));
    expect(reads).toBeLessThanOrEqual(7);
  });

  it('stops after a bounded number of reads', async () => {
    vi.useFakeTimers();
    const fetchTurnAttachments = vi.fn().mockResolvedValue(pendingTurn.attachments);
    renderSessionScreen({
      client: { cachedTranscript: () => [pendingTurn], transcript: async () => [pendingTurn], fetchTurnAttachments },
    });
    await act(() => vi.advanceTimersByTimeAsync(60 * 60_000));
    expect(fetchTurnAttachments).toHaveBeenCalledTimes(20);
  });

  it('drops a turn the gateway no longer has', async () => {
    const gone = vi.fn().mockRejectedValue(new GatewayError(404, 'not found'));
    expect(await readsInAMinute(gone)).toBe(1);
  });

  it('reads nothing while the app is hidden', async () => {
    vi.spyOn(document, 'visibilityState', 'get').mockReturnValue('hidden');
    const reads = await readsInAMinute(vi.fn().mockResolvedValue(pendingTurn.attachments));
    expect(reads).toBe(0);
  });
});
