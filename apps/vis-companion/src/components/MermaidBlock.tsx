import { memo, useEffect, useRef, useState, type ReactNode } from 'react';
import { canvasPngBlob } from '../lib/image-file';
import { CopyChip } from './ui';

// A `mermaid` fence is a PICTURE, and both surfaces paint it as one: the TUI
// ranks and draws the flowchart with box-drawing glyphs (`tui/mermaid.clj`),
// this component hands the same source to Pele and shows the SVG it draws.
//
// Pele renders synchronously, so each source is drawn once and kept: a block
// that mounts again (scrolling, a new message, a session switch) paints its
// cached picture in the first frame. Colours are CSS variables, so a new palette
// needs no new drawing. The library loads on FIRST USE only. When Pele cannot
// draw the fence (a syntax error mid-stream, a chunk that fails to load) the
// block falls back to the fence's own source, as the TUI does.

type Pele = typeof import('pele');

type Drawing = { svg: string } | { failed: true };

const FONT = "'JetBrains Mono Variable', ui-monospace, SFMono-Regular, monospace";

/** Drawings kept per source; a long transcript re-mounts its blocks often. */
const DRAWING_CACHE_SIZE = 200;

/** Pixels per diagram unit in a copied picture: sharp on a Retina paste target. */
const PICTURE_SCALE = 2;
/** iOS refuses a canvas above about 16.7 million pixels; stay under it. */
const PICTURE_MAX_PIXELS = 16_000_000;

let loadedPele: Pele | null = null;
let loadingPele: Promise<Pele> | null = null;

/** Load Pele once, after the web fonts: text widths are measured with them. */
function loadPele(): Promise<Pele> {
  loadingPele ??= (async () => {
    await globalThis.document?.fonts?.ready;
    loadedPele = await import('pele');
    return loadedPele;
  })().catch((cause: unknown) => {
    loadingPele = null;
    throw cause;
  });
  return loadingPele;
}

const drawings = new Map<string, Drawing>();
/** Each drawing needs ids of its own: two diagrams share one document. */
let sequence = 0;

/** The cached drawing of a source, or a new one. A failure is cached too. */
function drawDiagram(pele: Pele, source: string): Drawing {
  const cached = drawings.get(source);
  if (cached) {
    drawings.delete(source);
    drawings.set(source, cached);
    return cached;
  }
  let drawing: Drawing;
  try {
    sequence += 1;
    const { svg } = pele.render(source, {
      fontFamily: FONT,
      fontFamilyMono: FONT,
      fontSize: 14,
      idPrefix: `vis-diagram-${sequence}-`,
      // A diagram arrives with model output in it: no links and no remote images.
      links: false,
      images: false,
    });
    drawing = { svg };
  } catch {
    drawing = { failed: true };
  }
  drawings.set(source, drawing);
  if (drawings.size > DRAWING_CACHE_SIZE) {
    const oldest = drawings.keys().next().value;
    if (oldest !== undefined) drawings.delete(oldest);
  }
  return drawing;
}

/**
 * The SVG with every Pele colour variable replaced by its live value. An SVG
 * drawn as an image cannot see the page's CSS, so it would fall back to Pele's
 * own default palette.
 */
function bakeColors(svg: string, block: Element): string {
  const style = getComputedStyle(block);
  return svg.replace(
    /var\((--pele-[a-z0-9-]+),([^)]*)\)/gu,
    (_match, name: string, fallback: string) =>
      (style.getPropertyValue(name).trim() || fallback).replace(/"/gu, "'"),
  );
}

/** The drawn diagram as a PNG on the block's paper, from the SVG's own size. */
async function diagramPng(svg: string, block: Element): Promise<Blob> {
  const root = new DOMParser().parseFromString(bakeColors(svg, block), 'image/svg+xml').documentElement;
  const box = (root.getAttribute('viewBox') ?? '').split(/[\s,]+/u).map(Number);
  const width = box[2] > 0 ? box[2] : Number.parseFloat(root.getAttribute('width') ?? '') || 800;
  const height = box[3] > 0 ? box[3] : Number.parseFloat(root.getAttribute('height') ?? '') || 600;
  root.setAttribute('width', String(width));
  root.setAttribute('height', String(height));
  const image = new Image();
  image.src = `data:image/svg+xml;charset=utf-8,${encodeURIComponent(new XMLSerializer().serializeToString(root))}`;
  await image.decode();
  const scale = Math.min(PICTURE_SCALE, Math.sqrt(PICTURE_MAX_PIXELS / (width * height)));
  const canvas = document.createElement('canvas');
  canvas.width = Math.max(1, Math.round(width * scale));
  canvas.height = Math.max(1, Math.round(height * scale));
  const context = canvas.getContext('2d');
  if (!context) throw new Error('Could not prepare the picture.');
  context.fillStyle = getComputedStyle(block).getPropertyValue('--pele-bg').trim() || '#ffffff';
  context.fillRect(0, 0, canvas.width, canvas.height);
  context.drawImage(image, 0, 0, canvas.width, canvas.height);
  return canvasPngBlob(canvas);
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
  /** What to paint when Pele cannot draw this fence — the source itself. */
  fallback: ReactNode;
}) {
  const block = useRef<HTMLDivElement>(null);
  const [pele, setPele] = useState<Pele | null>(loadedPele);
  const [loadFailed, setLoadFailed] = useState(false);

  useEffect(() => {
    if (pele) return;
    let live = true;
    loadPele().then(
      (loaded) => {
        if (live) setPele(loaded);
      },
      () => {
        if (live) setLoadFailed(true);
      },
    );
    return () => {
      live = false;
    };
  }, [pele]);

  if (loadFailed) return <>{fallback}</>;
  // Nothing is drawn while the chunk loads: a fence that flashed its source and
  // then became a picture moved the whole transcript under the reader.
  if (!pele) return null;
  const drawing = drawDiagram(pele, source);
  if ('failed' in drawing) return <>{fallback}</>;

  return (
    <div
      ref={block}
      className={`relative overflow-hidden bg-code [--pele-accent:var(--color-accent)] [--pele-bg:var(--color-code)] [--pele-border:var(--color-code-edge)] [--pele-fg:var(--color-code-foreground)] [--pele-font-mono:var(--font-mono)] [--pele-font:var(--font-mono)] [--pele-line:var(--color-muted)] [--pele-muted:var(--color-muted)] [--pele-surface-alt:var(--color-code)] [--pele-surface:var(--color-code)] ${
        compact ? 'my-2' : 'my-3'
      } ${frameless ? '' : 'border border-code-edge'}`}
      data-mermaid-block=""
    >
      {!frameless && (
        <div
          className={`absolute right-0 z-10 flex bg-code pr-2 ${compact ? 'top-0 mouse:top-0.5' : 'top-0.5 mouse:top-1'}`}
        >
          <CopyChip
            type="image/png"
            value={async () => {
              if (!block.current) throw new Error('The diagram is gone.');
              return diagramPng(drawing.svg, block.current);
            }}
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
        // Pele escapes every label and filters links and styles; links and
        // images are off, and the markup is the SVG it just drew from this fence.
        dangerouslySetInnerHTML={{ __html: drawing.svg }}
      />
    </div>
  );
});
