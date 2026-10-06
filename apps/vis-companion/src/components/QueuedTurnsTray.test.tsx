// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import type { GatewayClient } from '../lib/gateway';
import type { QueuedTurn } from '../lib/types';
import { QueuedTurnsTray } from './QueuedTurnsTray';

afterEach(cleanup);

const queued: QueuedTurn[] = [
  {
    turnId: 'turn-2',
    request: 'Inspect the release manifest',
    preview: 'Inspect the release manifest',
    attachments: [{ filename: 'manifest.png', mediaType: 'image/png', sizeLabel: '24 KB' }],
  },
];

function gateway(methods: Partial<GatewayClient> = {}): GatewayClient {
  return {
    updateQueuedTurn: vi.fn().mockResolvedValue(undefined),
    markQueuedTurn: vi.fn().mockResolvedValue(undefined),
    sendQueueNow: vi.fn().mockResolvedValue(undefined),
    deleteQueuedTurn: vi.fn().mockResolvedValue(undefined),
    resumeQueue: vi.fn().mockResolvedValue(undefined),
    ...methods,
  } as unknown as GatewayClient;
}

const second: QueuedTurn = {
  turnId: 'turn-3',
  request: 'Summarize the failed checks',
  preview: 'Summarize the failed checks',
  attachments: [],
};

describe('queued turns tray', () => {
  it('shows image references once, without a redundant filename badge', () => {
    const preview = '[IMAGE #1] Inspect the screenshot';
    render(
      <QueuedTurnsTray
        client={gateway()}
        sid="session-1"
        queued={[{ ...queued[0], request: preview, preview }]}
        paused={null}
        running={false}
        onError={() => {}}
      />,
    );

    expect(screen.getByTitle('Tap to edit')).toHaveTextContent(preview);
    expect(screen.queryByText('manifest.png')).not.toBeInTheDocument();
    fireEvent.click(screen.getByTitle('Tap to edit'));
    expect(screen.getByLabelText('Edit queued message 1')).toHaveValue(preview);
  });

  it('keeps attachment-only and empty messages identifiable', () => {
    render(
      <QueuedTurnsTray
        client={gateway()}
        sid="session-1"
        queued={[
          { ...queued[0], request: '', preview: '' },
          { turnId: 'empty', request: '', preview: '', attachments: [] },
        ]}
        paused={null}
        running={false}
        onError={() => {}}
      />,
    );

    expect(screen.getByText('manifest.png')).toBeInTheDocument();
    expect(screen.getByText('(empty)')).toBeInTheDocument();
    expect(screen.getByText('manifest.png')).not.toHaveClass('border');
  });

  it('edits through the gateway without rewriting its row optimistically', async () => {
    const client = gateway();
    render(
      <QueuedTurnsTray
        client={client}
        sid="session-1"
        queued={queued}
        paused={null}
        running={false}
        onError={() => {}}
      />,
    );

    fireEvent.click(screen.getByTitle('Tap to edit'));
    const input = screen.getByLabelText('Edit queued message 1');
    fireEvent.change(input, {
      target: { value: 'Inspect the signed release manifest' },
    });
    fireEvent.keyDown(input, { key: 'Enter' });

    await waitFor(() =>
      expect(client.updateQueuedTurn).toHaveBeenCalledWith(
        'session-1',
        'turn-2',
        'Inspect the signed release manifest',
      ),
    );
    expect(screen.getByText('Inspect the release manifest')).toBeVisible();
    expect(screen.queryByText('manifest.png')).not.toBeInTheDocument();
  });

  it('owns removal and paused-queue recovery, including failures', async () => {
    const failure = new Error('queue changed first');
    const client = gateway({
      deleteQueuedTurn: vi.fn().mockRejectedValue(failure),
    });
    const onError = vi.fn();
    render(
      <QueuedTurnsTray
        client={client}
        sid="session-1"
        queued={queued}
        paused={{ held: 2, reason: 'turn_failed' }}
        running={false}
        onError={onError}
      />,
    );

    expect(screen.getByText('2 held · turn failed')).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: 'Continue queue' }));
    expect(client.resumeQueue).toHaveBeenCalledWith('session-1');

    fireEvent.click(screen.getByRole('button', { name: 'Remove queued message 1' }));
    expect(client.deleteQueuedTurn).toHaveBeenCalledWith('session-1', 'turn-2');
    expect(screen.getByText('Inspect the release manifest')).toBeVisible();
    await waitFor(() => expect(onError).toHaveBeenCalledWith('queue changed first'));
  });
  it('keeps the queued label and the shared square removal target', () => {
    render(
      <QueuedTurnsTray
        client={gateway()}
        sid="session-1"
        queued={queued}
        paused={null}
        running={false}
        onError={() => {}}
      />,
    );

    expect(screen.getByText('Queued · 1')).toBeVisible();
    const queue = screen.getByRole('region', { name: 'Queued messages' });
    for (const row of queue.querySelectorAll('[role="listitem"]')) {
      expect(row.className).toContain('py-0.5');
    }
    for (const remove of screen.getAllByRole('button', {
      name: /Remove queued message/,
    })) {
      expect(remove.className).toContain('size-8');
      expect(remove.className).toContain('mouse:size-7');
      expect(remove.className).toContain('after:size-11');
      expect(remove.className).toContain('rounded-none');
      expect(remove.className).toContain('bg-transparent');
      expect(remove.className).toContain('text-current');
      expect(remove.querySelector('span')).toBeNull();
      expect(remove.querySelector('svg')?.className.baseVal).toContain('size-2.5');
    }
  });

  it('keeps a long queue in a named keyboard-scrollable region', () => {
    render(
      <QueuedTurnsTray
        client={gateway()}
        sid="session-1"
        queued={Array.from({ length: 12 }, (_, index) => ({
          ...queued[0],
          turnId: `turn-${index}`,
          attachments: [],
        }))}
        paused={null}
        running={false}
        onError={() => {}}
      />,
    );

    const queue = screen.getByRole('region', { name: 'Queued messages' });
    expect(queue.tabIndex).toBe(0);
    expect(screen.getAllByRole('listitem')).toHaveLength(12);
  });
});

describe('send now', () => {
  it('marks one row through the gateway and keeps the row until the mirror arrives', async () => {
    const client = gateway();
    render(
      <QueuedTurnsTray
        client={client}
        sid="session-1"
        queued={[queued[0], second]}
        paused={null}
        running
        onError={() => {}}
      />,
    );

    const arrow = screen.getByRole('button', { name: 'Send queued message 2 now' });
    expect(arrow).toHaveAttribute('aria-pressed', 'false');
    fireEvent.click(arrow);
    expect(client.markQueuedTurn).toHaveBeenCalledTimes(1);
    expect(client.markQueuedTurn).toHaveBeenCalledWith('session-1', 'turn-3', 'next_iteration');
    // Busy, not rewritten: the gateway's `turn.queued.updated` paints the mark.
    expect(screen.getByText('Summarize the failed checks')).toBeVisible();
    expect(screen.getByRole('button', { name: 'Send queued message 1 now' })).toBeEnabled();
    expect(screen.queryByText('next step')).not.toBeInTheDocument();
    await waitFor(() =>
      expect(screen.getByRole('button', { name: 'Send queued message 2 now' })).toBeEnabled(),
    );
  });

  it('shows a marked row and unmarks it on the second press', () => {
    const client = gateway();
    render(
      <QueuedTurnsTray
        client={client}
        sid="session-1"
        queued={[queued[0], { ...second, deliver: 'next_iteration' }]}
        paused={null}
        running
        onError={() => {}}
      />,
    );

    expect(screen.getByText('next step')).toBeVisible();
    const keep = screen.getByRole('button', { name: 'Keep queued message 2 for the turn end' });
    expect(keep).toHaveAttribute('aria-pressed', 'true');
    fireEvent.click(keep);
    expect(client.markQueuedTurn).toHaveBeenCalledWith('session-1', 'turn-3', 'turn_end');
  });

  it('marks the whole queue from the header and disables it once all rows are marked', async () => {
    const client = gateway({ sendQueueNow: vi.fn().mockRejectedValue(new Error('gateway away')) });
    const onError = vi.fn();
    const { rerender } = render(
      <QueuedTurnsTray
        client={client}
        sid="session-1"
        queued={[queued[0], second]}
        paused={null}
        running
        onError={onError}
      />,
    );

    const all = screen.getByRole('button', { name: 'Send all queued messages now' });
    expect(all).toHaveTextContent(/^Send all now$/);
    fireEvent.click(all);
    expect(client.sendQueueNow).toHaveBeenCalledWith('session-1');
    await waitFor(() => expect(onError).toHaveBeenCalledWith('gateway away'));

    rerender(
      <QueuedTurnsTray
        client={client}
        sid="session-1"
        queued={[
          { ...queued[0], deliver: 'next_iteration' },
          { ...second, deliver: 'next_iteration' },
        ]}
        paused={null}
        running
        onError={onError}
      />,
    );
    expect(screen.getByRole('button', { name: 'Send all queued messages now' })).toBeDisabled();
  });

  // Regression, user report: on the dark queue band, "→ Send now" wore the page's ink, not the
  // band's own ink. It read at 1.44:1 in the default theme and vanished in tokyonight-day.
  // The header button now wears the accent fill with its own contrast pair.
  it('gives the header button its own ink pair on the dark band', () => {
    render(
      <QueuedTurnsTray
        client={gateway()}
        sid="session-1"
        queued={[queued[0], second]}
        paused={null}
        running
        onError={vi.fn()}
      />,
    );

    const all = screen.getByRole('button', { name: 'Send all queued messages now' });
    expect(all.parentElement).toHaveClass('bg-dialog-title', 'text-dialog-title-foreground');
    expect(all).toHaveClass('bg-accent', 'text-accent-foreground');
    expect(all).not.toHaveClass('text-white');
  });

  it('labels the send controls as short text buttons, without arrow glyphs', () => {
    render(
      <QueuedTurnsTray
        client={gateway()}
        sid="session-1"
        queued={[queued[0], { ...second, deliver: 'next_iteration' }]}
        paused={null}
        running
        onError={vi.fn()}
      />,
    );

    // User decision: the header sends the whole queue, a row sends only its own message.
    const all = screen.getByRole('button', { name: 'Send all queued messages now' });
    const rows = [
      screen.getByRole('button', { name: 'Send queued message 1 now' }),
      screen.getByRole('button', { name: 'Keep queued message 2 for the turn end' }),
    ];
    expect(all).toHaveTextContent(/^Send all now$/);
    for (const button of rows) expect(button).toHaveTextContent(/^Send it now$/);
    // The buttons fit the one-line row and band: they never make them taller.
    for (const button of [all, ...rows]) expect(button).toHaveClass('h-6', 'mouse:h-5');
    expect(screen.queryByText(/→/)).not.toBeInTheDocument();
  });

  it('offers no controls without a running turn and none on command rows', () => {
    const client = gateway();
    const { rerender } = render(
      <QueuedTurnsTray
        client={client}
        sid="session-1"
        queued={[queued[0], second]}
        paused={null}
        running={false}
        onError={() => {}}
      />,
    );

    expect(screen.queryByRole('button', { name: /now$/ })).not.toBeInTheDocument();

    rerender(
      <QueuedTurnsTray
        client={client}
        sid="session-1"
        queued={[
          { ...second, turnId: 'cmd-1', request: '/compact', preview: '/compact' },
          { ...second, turnId: 'cmd-2', request: '!git status', preview: '!git status' },
        ]}
        paused={null}
        running
        onError={() => {}}
      />,
    );
    expect(screen.queryByRole('button', { name: /^Send/ })).not.toBeInTheDocument();
    expect(screen.getAllByRole('button', { name: /Remove queued message/ })).toHaveLength(2);

    rerender(
      <QueuedTurnsTray
        client={client}
        sid="session-1"
        queued={[{ ...second, turnId: 'cmd-1', request: '/compact', preview: '/compact' }, second]}
        paused={null}
        running
        onError={() => {}}
      />,
    );
    expect(
      screen.queryByRole('button', { name: 'Send queued message 1 now' }),
    ).not.toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Send queued message 2 now' })).toBeEnabled();
    expect(screen.getByRole('button', { name: 'Send all queued messages now' })).toBeEnabled();
  });
});
