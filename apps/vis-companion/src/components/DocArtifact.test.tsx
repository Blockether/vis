import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';
import {
  DocAnnotateBar,
  DocCard,
  DocFrame,
  docSandbox,
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
      ['[Document: page.html HTML, 2 KB]', '/tmp/page.html', 'text/html'].join(
        '\n',
      ),
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
    const bare = renderToStaticMarkup(
      <DocCard body={fence} compact frameless />,
    );
    expect(framed).toContain('border border-code-edge');
    expect(bare.startsWith('<div class="my-2 flex w-full')).toBe(true);
  });

  it('never embeds the artifact itself — the transcript ships descriptors only', () => {
    expect(
      renderToStaticMarkup(<DocCard body={fence} compact />),
    ).not.toContain('<iframe');
  });
});
// Rasterising a document is the only way its content ever reaches a model: the
// bytes are clamped to `audience: "user"`, so what travels is a PICTURE of the
// page the human chose — and the page number has to be visible while choosing.
describe('DocAnnotateBar', () => {
  const baz = (props: Partial<Parameters<typeof DocAnnotateBar>[0]> = {}) =>
    renderToStaticMarkup(
      <DocAnnotateBar
        page={1}
        pageCount={0}
        busy={false}
        disabled={false}
        notice=""
        onPage={() => undefined}
        onCapture={() => undefined}
        {...props}
      />,
    );

  it('says which page of a PDF the capture will be', () => {
    const body = text(baz({ page: 3, pageCount: 12 }));
    expect(body).toContain('Page 3 of 12');
    expect(body).toContain('Draw on page 3');
  });

  it('offers no page picker for an artifact that has no pages', () => {
    const body = text(baz());
    expect(body).not.toContain('Page 1 of');
    expect(body).toContain('Draw on page');
  });

  it('stops at the first and the last page', () => {
    const first = baz({ page: 1, pageCount: 5 });
    expect(first).toContain('disabled="" aria-label="Previous page"');
    expect(first).not.toContain('disabled="" aria-label="Next page"');
    const last = baz({ page: 5, pageCount: 5 });
    expect(last).toContain('disabled="" aria-label="Next page"');
    expect(last).not.toContain('disabled="" aria-label="Previous page"');
  });

  it('reports the render in progress and what came of it', () => {
    expect(text(baz({ busy: true }))).toContain('Rendering');
    expect(baz({ busy: true })).toContain('disabled=""');
    expect(
      text(baz({ notice: 'Attached report-p3.png to your message.' })),
    ).toContain('Attached report-p3.png to your message.');
  });

  // A document whose page count is not known yet cannot be captured by page.
  it('waits for a PDF to be parsed before it will draw', () => {
    expect(baz({ disabled: true })).toContain('disabled=""');
  });

  // Density follows the pointer, not the width: these are the same touch-sized
  // hit boxes as every other control in the rail.
  it('keeps a touch-sized hit box', () => {
    expect(
      baz({ page: 2, pageCount: 4 }).match(/min-h-11/g)?.length,
    ).toBeGreaterThanOrEqual(3);
  });
});
