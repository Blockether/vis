import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';
import {
  DocCard,
  DocFrame,
  docKindLabel,
  docSandbox,
  isDocMedia,
  isPdfMedia,
  parseDocBlock,
} from './DocArtifact';

/** The block `vis_attach` emits for a document artifact: five header lines, no payload. */
const fence = [
  '[Document: report.pdf PDF, 1.2 MB]',
  '/tmp/vis-python/doc-1/report.pdf',
  'application/pdf',
  'report.pdf',
  '1.2 MB',
].join('\n');

/** Visible text of a rendered chunk: tags out, entities back. */
const text = (html: string) =>
  html
    .replace(/<[^>]+>/g, ' ')
    .replace(/&quot;/g, '"')
    .replace(/&#x27;/g, "'")
    .replace(/&gt;/g, '>')
    .replace(/&lt;/g, '<')
    .replace(/&amp;/g, '&');

describe('vis-doc fence', () => {
  it('splits the five header lines', () => {
    const artifact = parseDocBlock(fence);
    expect(artifact.summary).toBe('[Document: report.pdf PDF, 1.2 MB]');
    expect(artifact.path).toBe('/tmp/vis-python/doc-1/report.pdf');
    expect(artifact.mime).toBe('application/pdf');
    expect(artifact.name).toBe('report.pdf');
    expect(artifact.sizeLabel).toBe('1.2 MB');
  });

  it('falls back to the path basename when the name line is missing', () => {
    const artifact = parseDocBlock(
      ['[Document: page.html HTML, 2 KB]', '/tmp/page.html', 'text/html'].join('\n'),
    );
    expect(artifact.name).toBe('page.html');
    expect(artifact.sizeLabel).toBe('');
  });

  it('survives a body that carries no header at all', () => {
    const artifact = parseDocBlock('');
    expect(artifact.path).toBe('');
    expect(artifact.name).toBe('document');
  });
});

describe('document media types', () => {
  it('recognises the three types the engine keeps off the wire', () => {
    expect(isDocMedia('application/pdf')).toBe(true);
    expect(isDocMedia('text/html')).toBe(true);
    expect(isDocMedia('application/xhtml+xml')).toBe(true);
    expect(isDocMedia('TEXT/HTML; charset=utf-8')).toBe(true);
    expect(isDocMedia('image/png')).toBe(false);
    expect(isDocMedia('text/csv')).toBe(false);
    expect(isDocMedia(undefined)).toBe(false);
  });

  it('labels the kind the way the fence summary does', () => {
    expect(docKindLabel('application/pdf')).toBe('PDF');
    expect(docKindLabel('text/html')).toBe('HTML');
    expect(isPdfMedia('application/pdf')).toBe(true);
    expect(isPdfMedia('text/html')).toBe(false);
  });
});

// An attached page is UNTRUSTED markup. It renders in an iframe, which is its
// own document with its own CSS scope, and the sandbox is what makes that a
// security boundary: a blob: URL inherits the app's origin, so `allow-same-origin`
// would hand the artifact the app's storage and the gateway's bearer token.
describe('sandboxing', () => {
  it('never grants an artifact the app origin', () => {
    expect(docSandbox('text/html')).not.toContain('allow-same-origin');
    expect(docSandbox('application/pdf')).not.toContain('allow-same-origin');
  });

  it('runs no script for a page and allows only the browser PDF viewer', () => {
    expect(docSandbox('text/html')).toBe('');
    expect(docSandbox('application/xhtml+xml')).toBe('');
    expect(docSandbox('application/pdf')).toBe('allow-scripts');
  });

  it('paints the artifact inside a sandboxed frame', () => {
    const html = renderToStaticMarkup(
      <DocFrame url="blob:x" mime="text/html" name="page.html" />,
    );
    expect(html).toContain('<iframe');
    expect(html).toContain('sandbox=""');
    expect(html).toContain('title="page.html"');
    expect(html).not.toContain('allow-same-origin');
  });
});

describe('DocCard', () => {
  it('states what was produced and where it landed', () => {
    const body = text(renderToStaticMarkup(<DocCard body={fence} compact />));
    expect(body).toContain('PDF');
    expect(body).toContain('report.pdf');
    expect(body).toContain('application/pdf');
    expect(body).toContain('1.2 MB');
    expect(body).toContain('/tmp/vis-python/doc-1/report.pdf');
  });

  it('carries no frame of its own when a card already draws one', () => {
    const framed = renderToStaticMarkup(<DocCard body={fence} compact />);
    const bare = renderToStaticMarkup(<DocCard body={fence} compact frameless />);
    expect(framed).toContain('border border-code-edge');
    expect(bare.startsWith('<div class="my-2 flex w-full')).toBe(true);
  });

  it('never embeds the artifact itself — the transcript ships descriptors only', () => {
    expect(renderToStaticMarkup(<DocCard body={fence} compact />)).not.toContain('<iframe');
  });
});
