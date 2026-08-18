// @vitest-environment jsdom
// A settled run is opened from a FILE, so every case here folds a real record —
// the three lines `human-input.live-sink` writes — and reads the document that
// landed. The point of the screen is that a finished run stays readable without
// the terminal that ran it, and that reading it costs the phone a bounded
// picture rather than the whole log.
import { cleanup, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import {
  LiveArtifact,
  liveRecordFromEdges,
  liveVerdictLine,
  LIVE_RECORD_FOLD_LIMIT,
} from './LiveArtifact';
import liveArtifactSource from './LiveArtifact.tsx?raw';
import fixture from '../lib/live-view.fixture.json';
import { liveViewFromWire, type LiveRecord } from '../lib/live-view';
import type { GatewayClient } from '../lib/gateway';

afterEach(cleanup);

const openLine = JSON.stringify({ kind: 'open', at: 1, view: fixture });

const sealed = {
  ...fixture,
  nodes: [{ id: 'verdict', type: 'status', text: 'swept 3 hosts', tone: 'ok' }],
};

function closeLine(result: Record<string, unknown>): string {
  return JSON.stringify({ kind: 'close', at: 99, result });
}

/** A verdict with the engine's own view behind it — only the trailer keys vary. */
function ended(verdict: Partial<LiveRecord>): LiveRecord {
  const view = liveViewFromWire(fixture);
  if (!view) throw new Error('the engine fixture must be paintable');
  return { view, ...verdict };
}

function client(): GatewayClient {
  return {
    liveViewLog: vi.fn(async () => ({ node_id: 'x', from: 0, lines: [], total: 0 })),
  } as unknown as GatewayClient;
}

function serve(text: string) {
  vi.stubGlobal('fetch', async () => ({
    ok: true,
    blob: async () => new Blob([text]),
  }));
}

describe('how a finished run reads', () => {
  it('says how it ended in the words a person uses, and carries the note', () => {
    expect(liveVerdictLine(ended({ reason: 'completed' }))).toBe('finished');
    expect(liveVerdictLine(ended({ reason: 'timeout' }))).toBe('timed out');
    expect(
      liveVerdictLine(ended({ reason: 'interrupted', note: 'flaky on rerun' })),
    ).toBe('stopped by hand — flaky on rerun');
  });

  it('shows a reason it has no word for verbatim rather than as "ended"', () => {
    // The engine owns `live-reasons`; a vocabulary that grew there must reach the
    // phone as itself instead of being flattened into a word that says nothing.
    expect(liveVerdictLine(ended({ reason: 'evicted' }))).toBe('evicted');
    expect(liveVerdictLine(ended({}))).toBe('still recording');
  });
});

describe('a long record, read at its two ends', () => {
  it('paints the verdict picture without parsing a single patch line', () => {
    const head = `${openLine}\n{"kind":"patch","at":2,"patch":{"seq":1,"ops":[]}}`;
    const tail = `{"kind":"patch","at":8,"pa\n${closeLine({ reason: 'completed', is_completed: true, view: sealed })}`;
    const record = liveRecordFromEdges(head, tail);
    expect(record?.view.nodes.map((node) => node.id)).toEqual(['verdict']);
    expect(record?.reason).toBe('completed');
  });

  it('is null when neither end holds one complete line', () => {
    // A tail slice starts mid-line, so the FIRST line in it is only half a patch:
    // taking it would paint a picture out of a line the engine never finished.
    expect(liveRecordFromEdges(openLine, 'half a lin')).not.toBeNull();
    expect(liveRecordFromEdges('half a lin', 'half a lin')).toBeNull();
  });
});

describe('the artifact on screen', () => {
  it('paints the picture the run ended on, with nothing left running', async () => {
    serve([openLine, closeLine({ reason: 'completed', is_completed: true, view: sealed })].join('\n'));
    render(
      <LiveArtifact
        client={client()}
        sid="s1"
        url="blob:record"
        chrome={({ subtitle, body }) => (
          <div>
            <p>{subtitle}</p>
            {body}
          </div>
        )}
      />,
    );
    await waitFor(() => expect(screen.getByText('swept 3 hosts')).toBeTruthy());
    expect(screen.getByText('finished')).toBeTruthy();
    // A record cannot change again: no live region, and nothing to interrupt.
    expect(document.querySelector('[role="status"]')).toBeNull();
    expect(document.querySelector('[aria-live]')).toBeNull();
    expect(screen.queryByRole('button', { name: /interrupt/i })).toBeNull();
  });

  it('says so when the record cannot be read, instead of an empty picture', async () => {
    serve('not a record at all');
    render(
      <LiveArtifact
        client={client()}
        sid="s1"
        url="blob:broken"
        chrome={({ body }) => <div>{body}</div>}
      />,
    );
    await waitFor(() =>
      expect(screen.getByText("This run's record could not be read.")).toBeTruthy(),
    );
  });

  it('reads only the ends of a record past the fold limit', async () => {
    // The middle here is not JSON at all: a screen that folded the whole file
    // would stop at it and have no verdict to show. The verdict on screen IS the
    // proof that a 100 000-line run never came into this document.
    const middle = 'x'.repeat(LIVE_RECORD_FOLD_LIMIT);
    serve([openLine, middle, closeLine({ reason: 'failed', view: sealed })].join('\n'));
    render(
      <LiveArtifact
        client={client()}
        sid="s1"
        url="blob:huge"
        chrome={({ subtitle, body }) => (
          <div>
            <p>{subtitle}</p>
            {body}
          </div>
        )}
      />,
    );
    await waitFor(() => expect(screen.getByText('failed')).toBeTruthy());
    expect(screen.getByText('swept 3 hosts')).toBeTruthy();
  });

  it("is built out of the app's own controls", () => {
    expect(liveArtifactSource).toContain('<LiveViewPanel');
    expect(liveArtifactSource).not.toContain('<button');
    expect(liveArtifactSource).not.toContain('style=');
  });
});
