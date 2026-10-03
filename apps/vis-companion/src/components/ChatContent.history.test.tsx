// @vitest-environment jsdom
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { AssistantMessage, AttachmentRail } from './ChatContent';
import type { GatewayClient } from '../lib/gateway';
import type { TranscriptTurn } from '../lib/types';

// Large session switches must not download hidden steps before painting an answer.
describe('a windowed turn trace', () => {
  const newest = { id: 'i100', position: 100, assistant_prose: 'Latest progress' };
  const turn: TranscriptTurn = {
    turn_id: 't1', status: 'done', iterations: [newest], iterations_offset: 99,
    content: [{ id: 'answer', type: 'prose', markdown: 'The answer is ready.' }],
  };

  it('paints the answer without reading history, then loads earlier steps on request', async () => {
    let resolve!: (rows: typeof newest[]) => void;
    const turnTrace = vi.fn(() => new Promise<typeof newest[]>((done) => { resolve = done; }));
    const client = { turnTrace } as unknown as GatewayClient;
    const view = render(<AssistantMessage turn={turn} client={client} sid="s1" />);
    expect(screen.getByText('The answer is ready.')).toBeVisible();
    expect(screen.getByText('Latest progress')).toBeVisible();
    expect(turnTrace).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Show 99 earlier steps of this turn' }));
    expect(turnTrace).toHaveBeenCalledTimes(1);
    expect(turnTrace.mock.calls[0]).toEqual(['s1', 't1']);
    const updated = { ...newest, assistant_prose: 'Newer streamed progress' };
    view.rerender(<AssistantMessage turn={{ ...turn, iterations: [updated] }} client={client} sid="s1" />);
    await act(async () => resolve([{ id: 'i1', position: 1, assistant_prose: 'Earlier progress' }, newest]));
    expect(await screen.findByText('Earlier progress')).toBeVisible();
    expect(screen.getByText('Newer streamed progress')).toBeVisible();
    expect(screen.queryByText('Latest progress')).toBeNull();
    expect(screen.queryByRole('button', { name: /earlier steps/ })).toBeNull();
  });

  it('keeps the answer and retries a failed history read', async () => {
    const turnTrace = vi.fn().mockRejectedValueOnce(new Error('History is offline')).mockResolvedValueOnce([newest]);
    render(<AssistantMessage turn={turn} client={{ turnTrace } as unknown as GatewayClient} sid="s1" />);
    fireEvent.click(screen.getByRole('button', { name: 'Show 99 earlier steps of this turn' }));
    expect(await screen.findByText('History is offline')).toBeVisible();
    expect(screen.getByText('The answer is ready.')).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: 'Show 99 earlier steps of this turn' }));
    await waitFor(() => expect(screen.queryByText('History is offline')).toBeNull());
    expect(turnTrace).toHaveBeenCalledTimes(2);
  });

  it('keeps history reachable when the latest tool step alone exceeds the byte budget', async () => {
    const turnTrace = vi.fn().mockResolvedValue([newest]);
    render(<AssistantMessage turn={{ ...turn, iterations: [], iterations_offset: 100 }}
      client={{ turnTrace } as unknown as GatewayClient} sid="s1" />);
    expect(screen.getByText('The answer is ready.')).toBeVisible();
    expect(turnTrace).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Show 100 earlier steps of this turn' }));
    expect(await screen.findByText('Latest progress')).toBeVisible();
  });
});

// Hidden artifacts must not compete with the transcript during a session switch.
describe('attachment visibility', () => {
  afterEach(() => vi.unstubAllGlobals());

  it('loads an artifact only when its reserved slot approaches the viewport', async () => {
    let notify!: IntersectionObserverCallback;
    const observe = vi.fn();
    const disconnect = vi.fn();
    vi.stubGlobal('IntersectionObserver', class {
      constructor(callback: IntersectionObserverCallback) { notify = callback; }
      observe = observe;
      disconnect = disconnect;
    });
    const release = vi.fn();
    const attachmentUrl = vi.fn().mockResolvedValue('blob:visible');
    const retainAttachment = vi.fn(() => release);
    const client = { attachmentUrl, retainAttachment } as unknown as GatewayClient;
    const view = render(<AttachmentRail client={client} sid="s1" attachments={[
      { iteration_id: 'i1', index: 0, filename: 'earlier.png', media_type: 'image/png' },
    ]} />);
    expect(observe).toHaveBeenCalledOnce();
    expect(attachmentUrl).not.toHaveBeenCalled();
    expect(retainAttachment).not.toHaveBeenCalled();
    const target = observe.mock.calls[0][0];
    act(() => notify([{ target, isIntersecting: false } as IntersectionObserverEntry], {} as IntersectionObserver));
    expect(attachmentUrl).not.toHaveBeenCalled();
    act(() => notify([{ target, isIntersecting: true } as IntersectionObserverEntry], {} as IntersectionObserver));
    await waitFor(() => expect(screen.getByAltText('earlier.png')).toHaveAttribute('src', 'blob:visible'));
    expect(attachmentUrl).toHaveBeenCalledExactlyOnceWith('s1', 'i1', 0);
    expect(retainAttachment).toHaveBeenCalledExactlyOnceWith('s1', 'i1', 0);
    view.unmount();
    expect(release).toHaveBeenCalledOnce();
    expect(disconnect).toHaveBeenCalled();
  });
});
