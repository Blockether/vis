// @vitest-environment jsdom
import { act, render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it, vi } from 'vitest';
import { ArtifactsSheet, previewBlocks, previewLines } from './ArtifactsSheet';
import type { GatewayClient } from '../lib/gateway';
import type { SessionArtifact } from '../lib/artifacts';
import { ArtifactLinkContext } from '../lib/artifact-links';
import { dismissTopLayer } from '../lib/edge-back';
import { buildZip } from '../lib/zip-harness';

/** The bytes are never fetched in static markup: effects do not run. */
const client = {
  attachmentUrl: async () => 'blob:none',
  attachmentBlob: async (_sid: string, iterationId: string) =>
    iterationId === 'i6'
      ? new Blob(['name;email\nNatalia;natalia@example.com\n'], {
          type: 'text/csv',
        })
      : new Blob(['# Note\n\nA read that works.\n'], { type: 'text/markdown' }),
  retainAttachment: () => () => {},
  appViews: async () => [],
} as unknown as GatewayClient;

const artifact = (over: Partial<SessionArtifact>): SessionArtifact => ({
  key: over.key ?? 'i1:0',
  kind: 'image',
  name: 'revenue.png',
  media: 'PNG',
  mediaType: 'image/png',
  size: 2048,
  sizeLabel: '2.0KB',
  turn: 6,
  iterationId: 'i1',
  index: 0,
  version: 1,
  ...over,
});

const picture = artifact({});
const document = artifact({
  key: 'i2:0',
  kind: 'doc',
  name: 'q3-report.pdf',
  media: 'PDF',
  mediaType: 'application/pdf',
  size: 1024,
  sizeLabel: '1.0KB',
  turn: 5,
  iterationId: 'i2',
});
const note = artifact({
  key: 'i4:0',
  kind: 'doc',
  name: 'vis-issue-115-comment.md',
  media: 'MD',
  mediaType: 'text/markdown',
  size: 1900,
  sizeLabel: '1.9KB',
  turn: 1,
  iterationId: 'i4',
});
const table = artifact({
  key: 'i6:0',
  kind: 'doc',
  name: 'jobs.csv',
  media: 'CSV',
  mediaType: 'text/csv',
  size: 28,
  sizeLabel: '28B',
  turn: 2,
  iterationId: 'i6',
});

const sheet = (artifacts: SessionArtifact[]) => {
  const shell = globalThis.document.createElement('div');
  shell.setAttribute('data-viewport-shell', '');
  globalThis.document.body.append(shell);
  const view = render(
    <ArtifactsSheet client={client} sid="s1" artifacts={artifacts} onClose={() => {}} />,
  );
  const html = shell.querySelector('#artifacts-surface')?.outerHTML ?? '';
  view.unmount();
  shell.remove();
  return html;
};

/**
 * A gateway whose bytes can actually be read: the reader fetches the blob url.
 * The bytes arrive a TICK LATE, exactly as a real read does, so the overlay is
 * always painted once in its loading pass before the note itself can be read.
 */
const readable = () =>
  vi.stubGlobal(
    'fetch',
    vi.fn(async () => {
      await new Promise((resolve) => setTimeout(resolve, 5));
      return new Response(new TextEncoder().encode('# Note\n\nA read that works.\n'), {
        headers: { 'content-type': 'text/markdown' },
      });
    }),
  );

const readableTable = () =>
  vi.stubGlobal(
    'fetch',
    vi.fn(
      async () =>
        new Response('name;email\nNatalia;natalia@example.com\n', {
          headers: { 'content-type': 'text/csv' },
        }),
    ),
  );

/** Visible text of a rendered chunk: tags out, entities back. */
const text = (html: string) =>
  html
    .replace(/<[^>]+>/g, ' ')
    .replace(/&quot;/g, '"')
    .replace(/&#x27;/g, "'")
    .replace(/&gt;/g, '>')
    .replace(/&lt;/g, '<')
    .replace(/&amp;/g, '&');

describe('the artifacts sheet', () => {
  // Regression, user request: CSV tiles showed a generic page even though the artifact
  // already had enough readable bytes to show its own columns and first records.
  it('previews a CSV artifact in its tile', async () => {
    readableTable();
    const view = render(
      <ArtifactsSheet client={client} sid="s1" artifacts={[table]} onClose={() => {}} />,
    );

    await waitFor(() => expect(view.baseElement.textContent).toContain('Natalia'));
    expect(view.baseElement.textContent).toContain('name');
    expect(view.baseElement.textContent).toContain('email');
    expect(view.baseElement.textContent).toContain('natalia@example.com');
    expect(screen.queryByRole('grid', { name: 'jobs.csv' })).not.toBeInTheDocument();
    view.unmount();
    vi.unstubAllGlobals();
  });

  // Regression, user report ("why not sneakpeak! this should be visible too in
  // artifacts"): every document wore the same five grey bars, so two notes produced by
  // one session were told apart only by the filename under them.
  it('reads the head of a written note, blank lines dropped', () => {
    expect(previewLines('# Plan\n\n\nOne\nTwo\n')).toEqual(['# Plan', 'One', 'Two']);
    // Seven lines is the whole box: a log does not get to scroll a thumbnail, and an
    // eighth line only ever arrived to be eaten by the fade.
    expect(previewLines('a\nb\nc\nd\ne\nf\ng\nh\ni\nj')).toHaveLength(7);
    expect(previewLines('')).toEqual([]);
  });

  // Regression, user report ("make the MD look beautiful even in this small overview,
  // not verbose un-marked-down stuff"): the peek QUOTED the file, so a 96px tile spent
  // its width on `##`, `- [ ]`, `**` and a full URL, and two notes still looked alike.
  it('reads a note as the lines it is made of, never as its source', () => {
    const md = [
      '---',
      'title: front matter',
      '---',
      '# Plan',
      '',
      'Ship **it** with `care` and [a note](https://example.com).',
      '',
      '- first',
      '- [ ] second',
      '1. third',
      '',
      '> quoted',
      '',
      '```ts',
      'const a = 1;',
      '```',
      '---',
      '| a | b |',
      '| --- | --- |',
      '| 1 | 2 |',
      '',
    ].join('\n');
    expect(previewBlocks(md, 12)).toEqual([
      { kind: 'heading', text: 'Plan' },
      { kind: 'text', text: 'Ship it with care and a note.' },
      { kind: 'bullet', text: 'first', mark: '•' },
      { kind: 'bullet', text: 'second', mark: '○' },
      { kind: 'bullet', text: 'third', mark: '1.' },
      { kind: 'quote', text: 'quoted' },
      { kind: 'code', text: 'const a = 1;' },
      { kind: 'text', text: 'a · b' },
      { kind: 'text', text: '1 · 2' },
    ]);
    // The budget is the box: seven lines, whatever the file spends them on.
    expect(previewBlocks(md)).toHaveLength(7);
    expect(previewBlocks('')).toEqual([]);
  });

  // The same regression, on screen: the tile printed `# Note` in mono, and the kind
  // chip sat on top of the one line the fade had left readable.
  it("paints the note's title as a title, with no source and no chip over it", async () => {
    readable();
    const view = render(
      <ArtifactsSheet client={client} sid="s1" artifacts={[note]} onClose={() => {}} />,
    );
    await waitFor(() => expect(view.baseElement.textContent).toContain('A read that works.'));
    const html = view.baseElement.innerHTML;
    expect(html).not.toContain('# Note');
    // `MD` is the meta line's word, once, not a label stamped over the document.
    expect(view.baseElement.textContent?.match(/MD/g)).toHaveLength(1);
    view.unmount();
    vi.unstubAllGlobals();
  });

  it('keeps empty artifact filters reachable by keyboard', () => {
    render(<ArtifactsSheet client={client} sid="s1" artifacts={[]} onClose={() => {}} />);
    const filters = screen.getByRole('group', {
      name: 'Filter artifacts by kind',
    });
    expect(filters).toHaveAttribute('tabindex', '0');
    filters.focus();
    expect(filters).toHaveFocus();
  });

  // Live apps: the header counted the open apps, while the All chip said 0.
  it('counts the open apps under All, as the header does', async () => {
    const appClient = {
      ...client,
      appViews: async () => [
        { id: 'app-1', title: 'Counter', placement: 'attachment', seq: 1, nodes: [] },
        { id: 'app-2', title: 'Todo list', placement: 'attachment', seq: 1, nodes: [] },
      ],
    } as unknown as GatewayClient;
    const view = render(<ArtifactsSheet client={appClient} sid="s1" artifacts={[document]} onClose={() => {}} />);
    expect(await view.findByRole('button', { name: 'All, 3 artifacts' })).toBeEnabled();
    expect(view.getByRole('button', { name: 'Documents, 1 artifacts' })).toBeEnabled();
    view.unmount();
  });

});

describe('the artifacts sheet, paged', () => {
  const many = (count: number, size: number) =>
    Array.from({ length: count }, (_, at) =>
      artifact({
        key: `i${at}:0`,
        name: `shot-${at}.png`,
        iterationId: `i${at}`,
        size,
        sizeLabel: '64.0KB',
      }),
    );
  const tiles = (html: string) => html.match(/aria-label="Open /g)?.length ?? 0;

  it('paints one page of thumbnails, not two hundred downloads', () => {
    // Every tile that mounts fetches its own bytes, so opening this sheet on a
    // long session used to fire a request per artifact in a single tick.
    const html = sheet(many(30, 64 * 1024));
    expect(tiles(html)).toBe(12);
    expect(text(html)).toContain('Load 18 more');
    expect(html).toContain('aria-label="Load 18 more · 1.1MB of artifacts"');
  });

  it('pages on WEIGHT too, when a handful of artifacts is already heavy', () => {
    const html = sheet(many(6, 8 * 1024 * 1024));
    expect(tiles(html)).toBe(2);
    expect(text(html)).toContain('4 more');
  });

  it('says nothing about more when the whole production fits', () => {
    const html = sheet(many(4, 64 * 1024));
    expect(tiles(html)).toBe(4);
    expect(text(html)).not.toContain('more');
  });

});

describe('an artifact with a history', () => {
  const cut = (version: number) =>
    artifact({
      key: `i${version}:0`,
      name: 'chart.png',
      iterationId: `i${version}`,
      turn: version,
      version,
    });
  const threaded = { ...cut(3), versions: [cut(3), cut(2), cut(1)] };

  it('is ONE tile — the latest cut — not one tile per rewrite', () => {
    const html = sheet([threaded]);
    expect(html.match(/aria-label="Open /g)).toHaveLength(1);
    expect(html).toContain('produced in turn 3');
    expect(text(html)).toContain('chart.png');
  });

});
// Regression: an opened attachment was read in a letterbox. Every kind that owns
// its own scrolling — a clip, a note, a text file, a document — is handed the
// overlay's whole body (`fill`); only a list of rows keeps the padded scroller.
describe('an opened artifact', () => {
  // Regression, issue #1e96d5cf-c861-4fa9-88d7-4986c1ea37c7: a downloaded
  // Markdown plan stayed on `Loading…`; opening Share issued a second read and was
  // the only way to see bytes the client already held.
  it('reads downloaded text without fetching its object URL again', async () => {
    const directClient = {
      attachmentUrl: async () => 'blob:stalled',
      attachmentBlob: async () =>
        new Blob(['# Plan\n\nAlready downloaded.'], { type: 'text/markdown' }),
      retainAttachment: () => () => {},
      appViews: async () => [],
    } as unknown as GatewayClient;
    const stalledFetch = vi.fn(() => new Promise<Response>(() => {}));
    vi.stubGlobal('fetch', stalledFetch);
    const view = render(
      <ArtifactsSheet client={directClient} sid="s1" artifacts={[note]} onClose={() => {}} />,
    );

    try {
      await userEvent.click(
        screen.getByRole('button', { name: /^Open vis-issue-115-comment\.md/ }),
      );
      expect(await screen.findByText('Already downloaded.', { selector: 'p' })).toBeVisible();
      expect(stalledFetch).not.toHaveBeenCalled();
    } finally {
      view.unmount();
      vi.unstubAllGlobals();
    }
  });
  it('lists the files of a ZIP archive inside the artifact sheet', async () => {
    const buffer = await buildZip([
      { name: 'report/summary.txt', data: new TextEncoder().encode('All checks passed.') },
    ]);
    const zipClient = {
      attachmentUrl: async () => 'blob:zip',
      attachmentBlob: async () => new Blob([buffer], { type: 'application/zip' }),
      retainAttachment: () => () => {},
      appViews: async () => [],
    } as unknown as GatewayClient;
    const archive = artifact({
      key: 'i9:0',
      kind: 'file',
      name: 'results.zip',
      media: 'ZIP',
      mediaType: 'application/zip',
      iterationId: 'i9',
    });
    const view = render(
      <ArtifactsSheet
        client={zipClient}
        sid="s1"
        artifacts={[archive]}
        initialArtifact={archive}
        onClose={() => {}}
      />,
    );

    try {
      expect(await screen.findByRole('list', { name: 'Files in results.zip' })).toBeVisible();
      await userEvent.click(screen.getByText('report/summary.txt'));
      expect(await screen.findByText('All checks passed.')).toBeVisible();
      expect(screen.getByRole('button', { name: 'Download' })).toBeVisible();
    } finally {
      view.unmount();
    }
  });

  it('follows a named diff from an opened document inside the artifact sheet', async () => {
    const diffId = '7f477ec2-2c01-49bc-88eb-d0d7186179cf';
    const directClient = {
      attachmentUrl: async () => 'blob:report',
      attachmentBlob: async () =>
        new Blob(['# Report\n\n[Review diff](DIFF-ungroup-hover-hint.json)']),
      retainAttachment: () => () => {},
      appViews: async () => [],
    } as unknown as GatewayClient;
    const open = vi.fn();
    render(
      <ArtifactLinkContext.Provider
        value={{ byName: new Map([['DIFF-ungroup-hover-hint.json', diffId]]), open }}
      >
        <ArtifactsSheet
          client={directClient}
          sid="s1"
          artifacts={[note]}
          initialArtifact={note}
          onClose={() => {}}
        />
      </ArtifactLinkContext.Provider>,
    );

    await userEvent.click(await screen.findByRole('button', { name: 'Review diff' }));
    expect(open).toHaveBeenCalledExactlyOnceWith(diffId);
    expect(screen.queryByRole('dialog', { name: note.name })).toBeNull();
    expect(
      screen.getByRole('region', { name: 'Artifacts produced by the model' }),
    ).toBeInTheDocument();
  });

  // Reported with the back stroke, and the sheet has the same shape: Android's back is one
  // event for the shell, so an opened artifact used to carry the reader out of the session.
  // Back walks the ladder Escape walks — the opened cut first, then the sheet itself.
  it("takes down the opened artifact before the sheet, on the phone's back", () => {
    const onClose = vi.fn();
    const view = render(
      <ArtifactsSheet
        client={client}
        sid="s1"
        artifacts={[picture]}
        initialArtifact={picture}
        onClose={onClose}
      />,
    );
    expect(screen.getByRole('dialog', { name: 'revenue.png' })).toBeVisible();

    act(() => {
      dismissTopLayer();
    });

    expect(screen.queryByRole('dialog', { name: 'revenue.png' })).toBeNull();
    expect(onClose).not.toHaveBeenCalled();

    act(() => {
      dismissTopLayer();
    });

    expect(onClose).toHaveBeenCalledTimes(1);
    view.unmount();
  });
  async () => {
    const view = render(
      <ArtifactsSheet client={client} sid="s1" artifacts={[note]} onClose={() => {}} />,
    );
    await userEvent.click(screen.getByRole('button', { name: /^Open vis-issue-115-comment\.md/ }));
    // The overlay is painted TWICE: once while the bytes are still on the way
    // (a `Loading…` dialog) and again as a FRESH NODE once the note can be read
    // — a different component in that slot, so React drops the first node
    // instead of reusing it. Waiting for the prose before taking the dialog is
    // what keeps every assertion off the detached first paint; taking it any
    // earlier made this file fail on a loaded CI runner and pass alone.
    const prose = await screen.findByText('A read that works.', {
      selector: 'p',
    });
    const overlay = screen.getByRole('dialog', {
      name: 'vis-issue-115-comment.md',
    });
    return { view, overlay, prose };
  };
});
