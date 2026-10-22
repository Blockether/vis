import { memo, useEffect, useState, type ReactNode } from 'react';
import { canvasPngBlob } from '../lib/image-file';
import { CopyChip } from './ui';

// A `mermaid` fence is a PICTURE, and both surfaces paint it as one: the TUI
// ranks and draws the flowchart with box-drawing glyphs (`tui/mermaid.clj`),
// this component hands the same source to mermaid and shows the SVG it draws.
//
// The library is a big one, so it is imported on FIRST USE only — a conversation
// without a diagram never loads that chunk. When mermaid cannot draw the fence
// (a diagram type it rejects, a syntax error mid-stream, a chunk that fails to
// load) the block falls back to the fence's own source, exactly like the TUI
// paints the text when its renderer does not own the diagram.

type Rendered = { status: 'pending' } | { status: 'ready'; svg: string } | { status: 'failed' };

/** Pixels per diagram unit in a copied picture: sharp on a Retina paste target. */
const PICTURE_SCALE = 2;
/** iOS refuses a canvas above about 16.7 million pixels; stay under it. */
const PICTURE_MAX_PIXELS = 16_000_000;

/**
 * The drawn diagram as a PNG on the transcript's paper. The SVG gets the size of
 * its own viewBox, because mermaid sizes it to the column; an image without a
 * size draws at 0x0 in WebKit.
 */
async function diagramPng(svg: string, paper: string): Promise<Blob> {
  const root = new DOMParser().parseFromString(svg, 'image/svg+xml').documentElement;
  const box = (root.getAttribute('viewBox') ?? '').split(/[\s,]+/u).map(Number);
  const width = box[2] > 0 ? box[2] : Number.parseFloat(root.getAttribute('width') ?? '') || 800;
  const height = box[3] > 0 ? box[3] : Number.parseFloat(root.getAttribute('height') ?? '') || 600;
  root.setAttribute('width', String(width));
  root.setAttribute('height', String(height));
  root.removeAttribute('style');
  const image = new Image();
  image.src = `data:image/svg+xml;charset=utf-8,${encodeURIComponent(new XMLSerializer().serializeToString(root))}`;
  await image.decode();
  const scale = Math.min(PICTURE_SCALE, Math.sqrt(PICTURE_MAX_PIXELS / (width * height)));
  const canvas = document.createElement('canvas');
  canvas.width = Math.max(1, Math.round(width * scale));
  canvas.height = Math.max(1, Math.round(height * scale));
  const context = canvas.getContext('2d');
  if (!context) throw new Error('Could not prepare the picture.');
  context.fillStyle = paper;
  context.fillRect(0, 0, canvas.width, canvas.height);
  context.drawImage(image, 0, 0, canvas.width, canvas.height);
  return canvasPngBlob(canvas);
}

/** Each render needs an id of its own: mermaid looks the element up by it. */
let sequence = 0;

/** The live value behind a palette variable, so a diagram wears the session's theme. */
function paletteColor(token: string, fallback: string): string {
  if (typeof document === 'undefined') return fallback;
  const value = getComputedStyle(document.documentElement).getPropertyValue(token).trim();
  return value || fallback;
}

/**
 * Mermaid's `base` theme wired to the transcript's own code palette: paper, ink
 * and rules come from the theme variables, so the diagram sits in the message
 * like a code block rather than like a pasted screenshot.
 */
function diagramTheme(): Record<string, string> {
  const paper = paletteColor('--code-bg', '#f2ebdf');
  const ink = paletteColor('--code-fg', '#262626');
  const edge = paletteColor('--code-border', '#8c857a');
  const line = paletteColor('--dim', '#625d57');
  return {
    background: paper,
    primaryColor: paper,
    primaryTextColor: ink,
    primaryBorderColor: edge,
    secondaryColor: paper,
    secondaryTextColor: ink,
    secondaryBorderColor: edge,
    tertiaryColor: paper,
    tertiaryTextColor: ink,
    tertiaryBorderColor: edge,
    mainBkg: paper,
    nodeBorder: edge,
    nodeTextColor: ink,
    clusterBkg: paper,
    clusterBorder: edge,
    lineColor: line,
    arrowheadColor: line,
    textColor: ink,
    titleColor: ink,
    labelColor: ink,
    edgeLabelBackground: paper,
    fontSize: '14px',
  };
}

export const MermaidBlock = memo(function MermaidBlock({
  source,
  compact,
  frameless = false,
  fallback,
}: {
  /** The fence body, mermaid source as the author wrote it. */
  source: string;
  compact: boolean;
  /** Keep the spacing but drop the frame: an enclosing card already draws one. */
  frameless?: boolean;
  /** What to paint when mermaid cannot draw this fence — the source itself. */
  fallback: ReactNode;
}) {
  const [rendered, setRendered] = useState<Rendered>({ status: 'pending' });
  const [palette, setPalette] = useState(() =>
    typeof document === 'undefined' ? '' : (document.documentElement.dataset.theme ?? ''),
  );

  // The SVG bakes its colours in, so a new palette needs a new drawing. Nothing
  // else about the transcript changes, which is why this watches the one
  // attribute `applyTheme` writes instead of re-rendering on every paint.
  useEffect(() => {
    if (typeof MutationObserver === 'undefined') return;
    const root = document.documentElement;
    const watch = new MutationObserver(() => setPalette(root.dataset.theme ?? ''));
    watch.observe(root, { attributes: true, attributeFilter: ['data-theme'] });
    return () => watch.disconnect();
  }, []);

  useEffect(() => {
    let live = true;
    const draw = async () => {
      try {
        const { default: mermaid } = await import('mermaid');
        mermaid.initialize({
          startOnLoad: false,
          // Labels are sanitized and no HTML is honoured inside them: a diagram
          // arrives with model output in it.
          securityLevel: 'strict',
          theme: 'base',
          themeVariables: diagramTheme(),
          fontFamily: "'JetBrains Mono Variable', ui-monospace, SFMono-Regular, monospace",
          flowchart: { htmlLabels: false, useMaxWidth: true },
        });
        if (!(await mermaid.parse(source, { suppressErrors: true }))) {
          if (live) setRendered({ status: 'failed' });
          return;
        }
        sequence += 1;
        const { svg } = await mermaid.render(`vis-diagram-${sequence}`, source);
        if (live) setRendered({ status: 'ready', svg });
      } catch {
        if (live) setRendered({ status: 'failed' });
      }
    };
    void draw();
    return () => {
      live = false;
    };
  }, [source, palette]);

  if (rendered.status === 'failed') return <>{fallback}</>;
  // Nothing is drawn while the chunk loads: a fence that flashed its source and
  // then became a picture moved the whole transcript under the reader.
  if (rendered.status === 'pending') return null;

  return (
    <div
      className={`relative overflow-hidden bg-code ${compact ? 'my-2' : 'my-3'} ${
        frameless ? '' : 'border border-code-edge'
      }`}
      data-mermaid-block=""
    >
      {!frameless && (
        <div
          className={`absolute right-0 z-10 flex bg-code pr-2 ${compact ? 'top-0 mouse:top-0.5' : 'top-0.5 mouse:top-1'}`}
        >
          <CopyChip
            type="image/png"
            value={() => diagramPng(rendered.svg, paletteColor('--code-bg', '#f2ebdf'))}
            label="Copy diagram as image"
          />
          <CopyChip value={source} label="Copy diagram source" />
        </div>
      )}
      <div
        className={`max-w-full overflow-x-auto overscroll-x-contain px-3 text-center ${
          compact ? 'py-2' : 'py-2.5'
        } [&_svg]:mx-auto [&_svg]:h-auto [&_svg]:max-w-full`}
        role="img"
        aria-label="Diagram"
        tabIndex={0}
        // mermaid sanitizes the labels it draws (securityLevel `strict`), and the
        // markup here is the SVG it just produced from this fence.
        dangerouslySetInnerHTML={{ __html: rendered.svg }}
      />
    </div>
  );
});
