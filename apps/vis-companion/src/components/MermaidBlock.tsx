import { memo, useEffect, useState, type ReactNode } from 'react';
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
          className={`absolute right-0 z-10 bg-code pr-2 ${compact ? 'top-0 mouse:top-0.5' : 'top-0.5 mouse:top-1'}`}
        >
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
