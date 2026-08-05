import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';
import { ArtifactsChip, ArtifactsSheet } from './ArtifactsSheet';
import type { GatewayClient } from '../lib/gateway';
import type { SessionArtifact } from '../lib/artifacts';

/** The bytes are never fetched in static markup: effects do not run. */
const client = {
  attachmentUrl: async () => 'blob:none',
  retainAttachment: () => () => {},
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
  tool: 'python_execution',
  iterationId: 'i1',
  index: 0,
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
const recorded = artifact({
  key: 'i3:0',
  kind: 'file',
  name: 'build.log',
  media: 'LOG',
  mediaType: 'text/plain',
  size: 1024,
  sizeLabel: '1.0KB',
  turn: 4,
  tool: 'shell',
  iterationId: 'i3',
});

const sheet = (artifacts: SessionArtifact[]) =>
  renderToStaticMarkup(
    <ArtifactsSheet
      client={client}
      sid="s1"
      artifacts={artifacts}
      onClose={() => {}}
    />,
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

describe('the artifacts chip', () => {
  it('costs a session that produced nothing exactly nothing', () => {
    expect(
      renderToStaticMarkup(
        <ArtifactsChip count={0} open={false} onToggle={() => {}} />,
      ),
    ).toBe('');
  });

  it('says what it owns and whether that surface is open', () => {
    const html = renderToStaticMarkup(
      <ArtifactsChip count={12} open={false} onToggle={() => {}} />,
    );
    expect(html).toContain('aria-expanded="false"');
    expect(html).toContain('aria-controls="artifacts-surface"');
    expect(html).toContain('aria-label="12 artifacts produced by the model"');
    // The word does not fit a phone header, so the pixels carry `▣ 12`.
    expect(text(html)).toContain('12');
    expect(html).toContain('hidden sm:inline');
  });

  it('never shrinks under a finger, only under a cursor', () => {
    const html = renderToStaticMarkup(
      <ArtifactsChip count={3} open onToggle={() => {}} />,
    );
    expect(html).toContain('min-h-11');
    expect(html).toContain('mouse:min-h-6');
    expect(html).toContain('aria-expanded="true"');
  });
});

describe('the artifacts sheet', () => {
  it('is the region the chip claims to control, over the transcript', () => {
    const html = sheet([picture, document, recorded]);
    expect(html).toContain('id="artifacts-surface"');
    expect(html).toContain('role="region"');
    expect(html).toContain('absolute inset-0');
  });

  it('counts the production and adds up what it weighs', () => {
    expect(text(sheet([picture, document, recorded]))).toContain(
      '3 produced by the model · 4.0KB',
    );
  });

  it('draws a filter with nothing behind it disabled, never hidden', () => {
    const html = sheet([document]);
    expect(html).toContain('aria-label="Pictures, 0 artifacts"');
    expect(html).toContain('aria-label="Documents, 1 artifacts"');
    // The strip keeps its shape: a disabled chip is still four chips wide.
    expect(html.match(/aria-pressed=/g)).toHaveLength(4);
    expect(html).toContain('disabled=""');
  });

  it('opens what it can read and refuses to fake the rest', () => {
    const html = sheet([picture, document, recorded]);
    expect(html).toContain(
      'aria-label="Open revenue.png, PNG, 2.0KB, produced in turn 6 by python_execution"',
    );
    expect(html).toContain('aria-label="Open q3-report.pdf, PDF, 1.0KB');
    // A recorded file has no reader in the app, so it is not a control at all.
    expect(html).not.toContain('build.log, LOG');
    expect(text(html)).toContain('build.log');
    expect(html.match(/<button/g)).toHaveLength(
      // close + four filters + two openable tiles
      7,
    );
  });

  it('carries the provenance that makes an artifact citable', () => {
    const visible = text(sheet([picture]));
    expect(visible).toContain('revenue.png');
    expect(visible).toContain('PNG · 2.0KB · turn 6');
  });

  it('says so when a filter has nothing behind it', () => {
    expect(text(sheet([]))).toContain('Nothing of that kind in this session.');
  });
});
