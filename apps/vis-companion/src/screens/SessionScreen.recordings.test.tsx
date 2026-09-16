// @vitest-environment jsdom
import { act, screen, waitFor } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';
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
