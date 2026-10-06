// @vitest-environment jsdom
import { act, render } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { IterationTrace } from './ChatContent';
import type { GatewayClient } from '../lib/gateway';
import type { TranscriptIteration } from '../lib/types';

// A queued message the human sent with `→` (PLAN-queue-send-now, Task 4) lands at the
// start of a step. The trace paints it as a `You` band above that step's work, with
// the step it reached, on both the live bubble and the replayed transcript.
describe('a queued message delivered into the turn', () => {
  const iterations: TranscriptIteration[] = [
    { position: 1, thinking: 'Reading the failing test.', assistant_prose: 'I read the test.' },
    {
      position: 2,
      user_input: [{ queued_turn_id: 'q-1', request: 'Also check the lint config.' }],
      thinking: 'Checking the lint config too.',
    },
  ];

  const bands = (container: HTMLElement) =>
    Array.from(container.querySelectorAll('[data-transcript-user-input]'));

  it('paints the message above the reasoning of the step it reached', () => {
    const { container } = render(<IterationTrace iterations={iterations} whole />);

    const [band] = bands(container);
    // User report: the band said `step N`; the transcript counts iterations, so it says `iter N`.
    expect(band?.textContent).toContain('You · sent now · iter 2');
    expect(band?.textContent).toContain('Also check the lint config.');
    const text = container.textContent ?? '';
    expect(text.indexOf('Also check the lint config.')).toBeLessThan(
      text.indexOf('Checking the lint config too.'),
    );
    expect(text.indexOf('I read the test.')).toBeLessThan(text.indexOf('Also check the lint'));
  });

  it('shows a step that carries only the delivered message', () => {
    const { container } = render(
      <IterationTrace
        iterations={[{ position: 3, user_input: [{ request: 'Stop after the tests.' }] }]}
        whole
      />,
    );

    expect(bands(container)).toHaveLength(1);
    expect(container.textContent).toContain('iter 3');
    expect(container.textContent).toContain('Stop after the tests.');
  });

  it('prefers the paste-collapsed text and names the images that came along', () => {
    const { container } = render(
      <IterationTrace
        iterations={[
          {
            position: 2,
            user_input: [
              {
                request: 'Compare with this\n```\nraw paste\n```',
                display_request: 'Compare with this [paste 3 lines]',
                attachment_previews: [{ filename: 'shot.png', media_type: 'image/png' }],
              },
            ],
          },
        ]}
        whole
      />,
    );

    const [band] = bands(container);
    expect(band?.textContent).toContain('Compare with this [paste 3 lines]');
    expect(band?.textContent).not.toContain('raw paste');
    expect(band?.textContent).toContain('shot.png');
  });

  describe('with the pictures it carried', () => {
    afterEach(() => {
      vi.useRealTimers();
    });

    const sent: TranscriptIteration[] = [
      {
        position: 2,
        user_input: [
          {
            queued_turn_id: 'q-1',
            request: 'Compare with this.',
            attachment_previews: [{ filename: 'shot.png', media_type: 'image/png' }],
          },
        ],
      },
    ];
    const shot = {
      id: 'a-1',
      source: 'user',
      filename: 'shot.png',
      media_type: 'image/png',
      base64: 'QUJD',
    };
    const settle = (ms: number) =>
      act(async () => {
        await vi.advanceTimersByTimeAsync(ms);
      });

    it('paints the stored pictures and asks again while the step stores them', async () => {
      vi.useFakeTimers();
      const fetchTurnAttachments = vi.fn().mockResolvedValueOnce([]).mockResolvedValue([shot]);
      const client = { fetchTurnAttachments } as unknown as GatewayClient;
      const { container } = render(<IterationTrace iterations={sent} client={client} sid="s-1" whole />);

      await settle(0);
      const [band] = bands(container);
      expect(band?.textContent).toContain('🖼 shot.png');
      expect(band?.querySelector('img')).toBeNull();

      await settle(1000);
      expect(fetchTurnAttachments).toHaveBeenCalledTimes(2);
      expect(fetchTurnAttachments).toHaveBeenCalledWith('s-1', 'q-1', expect.any(AbortSignal));
      expect(band?.querySelector('img')?.getAttribute('src')).toBe('data:image/png;base64,QUJD');
      expect(band?.textContent).not.toContain('🖼 shot.png');
    });

    it('keeps the chips when the gateway has no pictures', async () => {
      vi.useFakeTimers();
      const fetchTurnAttachments = vi.fn().mockResolvedValue([]);
      const client = { fetchTurnAttachments } as unknown as GatewayClient;
      const { container } = render(<IterationTrace iterations={sent} client={client} sid="s-1" whole />);

      await settle(10_000);
      expect(fetchTurnAttachments).toHaveBeenCalledTimes(4);
      expect(bands(container)[0]?.textContent).toContain('🖼 shot.png');
    });
  });

  it('paints nothing extra for a step without delivered input', () => {
    const { container } = render(<IterationTrace iterations={[iterations[0]]} whole />);

    expect(bands(container)).toHaveLength(0);
  });
});
