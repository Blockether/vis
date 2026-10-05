// @vitest-environment jsdom
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { AssistantMessage, AttachmentRail, earlierStepsPendingInView } from './ChatContent';
import type { GatewayClient } from '../lib/gateway';
import { setStepsSummarized } from '../lib/transcript-display';
import type { TranscriptIteration, TranscriptTurn } from '../lib/types';

/** The test decides when the trace nears the reader. Only marks above loading content hear it. */
function stubNearness() {
  const watchers: { notify: IntersectionObserverCallback; targets: Element[] }[] = [];
  vi.stubGlobal('IntersectionObserver', class {
    private readonly watcher: { notify: IntersectionObserverCallback; targets: Element[] };
    constructor(notify: IntersectionObserverCallback) {
      this.watcher = { notify, targets: [] };
      watchers.push(this.watcher);
    }
    observe(target: Element) { this.watcher.targets.push(target); }
    unobserve() {}
    disconnect() { this.watcher.targets = []; }
    takeRecords() { return []; }
  });
  return (isIntersecting: boolean) => act(() => {
    for (const { notify, targets } of watchers) {
      const marks = targets.filter((target) => target.matches('[data-anchor="skip"]'));
      if (!marks.length) continue;
      notify(marks.map((target) => ({ target, isIntersecting }) as IntersectionObserverEntry), {} as IntersectionObserver);
    }
  });
}

/** A client that reads a turn trace, and holds `cached` from an earlier visit. */
function traceClient(turnTrace: unknown, cached: TranscriptIteration[] | null = null) {
  return { turnTrace, cachedTurnTrace: () => cached } as unknown as GatewayClient;
}

// Large session switches must not download hidden steps before painting an answer.
describe('a windowed turn trace', () => {
  const newest = { id: 'i100', position: 100, assistant_prose: 'Latest progress' };
  const earliest = { id: 'i1', position: 1, assistant_prose: 'Earlier progress' };
  const turn: TranscriptTurn = {
    turn_id: 't1', status: 'done', iterations: [newest], iterations_offset: 99,
    content: [{ id: 'answer', type: 'prose', markdown: 'The answer is ready.' }],
  };

  afterEach(() => {
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
    setStepsSummarized(true);
  });

  // Regression, user request: in Compact mode the digests are the fold. A control that
  // counted 26 hidden steps opened as two notes and two digests.
  it('paints the answer without reading history, then reads earlier steps as the trace nears', async () => {
    const near = stubNearness();
    let resolve!: (rows: typeof newest[]) => void;
    const turnTrace = vi.fn(() => new Promise<typeof newest[]>((done) => { resolve = done; }));
    const client = traceClient(turnTrace);
    const view = render(<AssistantMessage turn={turn} client={client} sid="s1" />);
    expect(screen.getByText('The answer is ready.')).toBeVisible();
    expect(screen.getByText('Latest progress')).toBeVisible();
    expect(screen.queryByRole('button', { name: /earlier step/ })).toBeNull();
    near(false);
    expect(turnTrace).not.toHaveBeenCalled();
    near(true);
    near(true);
    expect(turnTrace).toHaveBeenCalledExactlyOnceWith('s1', 't1', expect.any(AbortSignal));
    const updated = { ...newest, assistant_prose: 'Newer streamed progress' };
    view.rerender(<AssistantMessage turn={{ ...turn, iterations: [updated] }} client={client} sid="s1" />);
    await act(async () => resolve([earliest, newest]));
    expect(await screen.findByText('Earlier progress')).toBeVisible();
    expect(screen.getByText('Newer streamed progress')).toBeVisible();
    expect(screen.queryByText('Latest progress')).toBeNull();
    expect(view.container.querySelector('[data-anchor="skip"]')).toBeNull();
  });

  it('keeps the answer and retries a failed history read on request', async () => {
    const near = stubNearness();
    const turnTrace = vi.fn().mockRejectedValueOnce(new Error('History is offline')).mockResolvedValueOnce([newest]);
    render(<AssistantMessage turn={turn} client={traceClient(turnTrace)} sid="s1" />);
    near(true);
    expect(await screen.findByText('History is offline')).toBeVisible();
    expect(screen.getByText('The answer is ready.')).toBeVisible();
    near(true);
    expect(turnTrace).toHaveBeenCalledTimes(1);
    fireEvent.click(screen.getByRole('button', { name: 'Try loading earlier steps again' }));
    await waitFor(() => expect(screen.queryByText('History is offline')).toBeNull());
    expect(turnTrace).toHaveBeenCalledTimes(2);
  });

  it('keeps history reachable when the latest tool step alone exceeds the byte budget', async () => {
    const near = stubNearness();
    const turnTrace = vi.fn().mockResolvedValue([newest]);
    render(<AssistantMessage turn={{ ...turn, iterations: [], iterations_offset: 100 }}
      client={traceClient(turnTrace)} sid="s1" />);
    expect(screen.getByText('The answer is ready.')).toBeVisible();
    expect(turnTrace).not.toHaveBeenCalled();
    near(true);
    expect(await screen.findByText('Latest progress')).toBeVisible();
  });

  it('stops reading history when the turn leaves the screen', () => {
    const near = stubNearness();
    const turnTrace = vi.fn(() => new Promise<never>(() => {}));
    const view = render(<AssistantMessage turn={turn} client={traceClient(turnTrace)} sid="s1" />);
    near(true);
    const signal = (turnTrace.mock.calls[0] as unknown[])[2] as AbortSignal;
    expect(signal.aborted).toBe(false);
    view.unmount();
    expect(signal.aborted).toBe(true);
  });

  // Regression, user request: no step mode shows an earlier-steps control, as in the TUI.
  it('reads earlier steps without a control when every step shows separately', async () => {
    setStepsSummarized(false);
    const near = stubNearness();
    const turnTrace = vi.fn().mockResolvedValue([earliest, newest]);
    render(<AssistantMessage turn={turn} client={traceClient(turnTrace)} sid="s1" />);
    expect(screen.queryByRole('button', { name: /earlier step/ })).toBeNull();
    expect(turnTrace).not.toHaveBeenCalled();
    near(true);
    expect(await screen.findByText('Earlier progress')).toBeVisible();
    expect(turnTrace).toHaveBeenCalledOnce();
  });

  // Regression, user report: opening a session grew a long turn in view a moment later.
  it('paints the steps that an earlier visit read without reading them again', () => {
    const near = stubNearness();
    const turnTrace = vi.fn();
    const view = render(
      <AssistantMessage turn={turn} client={traceClient(turnTrace, [earliest, newest])} sid="s1" />,
    );
    expect(screen.getByText('Earlier progress')).toBeVisible();
    expect(screen.getByText('Latest progress')).toBeVisible();
    expect(view.container.querySelector('[data-earlier-steps]')).toBeNull();
    near(true);
    expect(turnTrace).not.toHaveBeenCalled();
  });

  it('reports a turn in view until its earlier steps land', async () => {
    const near = stubNearness();
    let resolve!: (rows: typeof newest[]) => void;
    const turnTrace = vi.fn(() => new Promise<typeof newest[]>((done) => { resolve = done; }));
    render(
      <div data-testid="reader">
        <AssistantMessage turn={turn} client={traceClient(turnTrace)} sid="s1" />
      </div>,
    );
    const reader = screen.getByTestId('reader');
    let markTop = 300;
    vi.spyOn(Element.prototype, 'getBoundingClientRect').mockImplementation(function (this: Element) {
      const top = this === reader ? 0 : markTop;
      return { top, bottom: top + 800, left: 0, right: 400, width: 400, height: 800, x: 0, y: top } as DOMRect;
    });
    expect(earlierStepsPendingInView(reader)).toBe(true);
    // Steps that land above the view move nothing that the reader sees.
    markTop = -40;
    expect(earlierStepsPendingInView(reader)).toBe(false);
    markTop = 300;
    near(true);
    await act(async () => resolve([earliest, newest]));
    expect(screen.getByText('Earlier progress')).toBeVisible();
    expect(earlierStepsPendingInView(reader)).toBe(false);
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
