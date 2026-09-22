import { Children, Fragment, useEffect, useRef, useState, type ReactNode } from 'react';
import type { Prepared } from '@kitlangton/justice';

// Justice currently handles single-font, space-delimited LTR text. Rich Markdown,
// bidi/CJK, preserved whitespace and very long paragraphs keep native wrapping.
function plainText(children: ReactNode): string | null {
  const parts = Children.toArray(children);
  if (!parts.every((part) => typeof part === 'string')) return null;
  const text = parts.join('');
  if (
    !text ||
    text !== text.trim() ||
    /[\u00a0\u200e\u200f\u202a-\u202e\u2066-\u2069]/u.test(text) ||
    (text.match(/[^ \t\r\n\f]+/g)?.length ?? 0) > 400 ||
    [...text.matchAll(/\p{Letter}/gu)].some(
      ([letter]) => !/[\p{Script=Latin}\p{Script=Greek}\p{Script=Cyrillic}]/u.test(letter),
    )
  )
    return null;
  return text;
}

type ProseLine = { text: string; separator: string; wordSpacing: number; tracking: number };
type Composition = { text: string; lines: ProseLine[] };

/**
 * A width arrives as a BURST — the desk rail riding off its seam, a window dragged by
 * its corner — and the width a paragraph is composed for is its last frame. Composing
 * on each of the others solves and rewrites every line for a width nobody reads.
 */
const SETTLE_MS = 80;

/** Progressive paragraph composition; React keeps ownership of all visible text. */
export function JustifiedProse({
  as: Tag = 'p',
  children,
  className,
  enabled = true,
}: {
  as?: 'p' | 'li';
  children: ReactNode;
  className?: string;
  enabled?: boolean;
}) {
  const ref = useRef<HTMLParagraphElement & HTMLLIElement>(null);
  const text = enabled ? plainText(children) : null;
  const [composition, setComposition] = useState<Composition | null>(null);

  useEffect(() => {
    const element = ref.current;
    if (
      !element ||
      text === null ||
      typeof Intl.Segmenter !== 'function' ||
      typeof ResizeObserver !== 'function'
    )
      return;
    let disposed = false;
    let frame = 0;
    let prepared: Prepared | undefined;
    let fontKey = '';
    let lastWidth = 0;
    /** The width the last resize reported, and the wait for the box to stop moving. */
    let seenWidth = 0;
    let settleTimer = 0;
    /** True while the box is moving and the paragraph is left to wrap natively. */
    let riding = false;
    let engine: typeof import('@kitlangton/justice') | undefined;
    const words = [...text.matchAll(/[^ \t\r\n\f]+/g)];

    /**
     * Replacing line nodes during a native selection would discard the range. Leave
     * the composition exactly as it stands and refit once the range is cleared,
     * instead of disrupting copying or annotation.
     */
    const isSelected = () => {
      const selection = window.getSelection();
      return !!selection && !selection.isCollapsed && selection.containsNode(element, true);
    };

    const fit = () => {
      if (disposed || !engine) return;
      if (isSelected()) return;
      const style = getComputedStyle(element);
      const width =
        parseFloat(style.width) -
        (style.boxSizing === 'border-box'
          ? parseFloat(style.paddingLeft) +
            parseFloat(style.paddingRight) +
            parseFloat(style.borderLeftWidth) +
            parseFloat(style.borderRightWidth)
          : 0);
      if (
        !(width > 0) ||
        style.direction !== 'ltr' ||
        style.writingMode !== 'horizontal-tb' ||
        style.whiteSpace !== 'normal' ||
        style.textTransform !== 'none' ||
        parseFloat(style.textIndent) !== 0
      ) {
        lastWidth = 0;
        setComposition(null);
        return;
      }
      const nextFont = [
        style.fontFamily,
        style.fontSize,
        style.fontWeight,
        style.fontStyle,
        style.fontStretch,
        style.fontFeatureSettings,
        style.fontVariationSettings,
        style.fontOpticalSizing,
      ].join('|');
      if (prepared && fontKey === nextFont && lastWidth === width) return;
      if (!prepared || fontKey !== nextFont) {
        // DOM measurement inherits the exact face, variable axes and OpenType features.
        // A canvas font shorthand alone loses the app's character-variant settings.
        const probe = document.createElement('span');
        Object.assign(probe.style, {
          position: 'fixed',
          visibility: 'hidden',
          pointerEvents: 'none',
          whiteSpace: 'pre',
          letterSpacing: '0px',
          wordSpacing: '0px',
          fontKerning: 'none',
          fontVariantLigatures: 'none',
        });
        probe.setAttribute('aria-hidden', 'true');
        element.appendChild(probe);
        try {
          prepared = engine.prepare(text, (word) => {
            probe.textContent = word;
            return probe.getBoundingClientRect().width;
          });
          fontKey = nextFont;
        } finally {
          probe.remove();
        }
      }
      lastWidth = width;
      const layout = engine.solve(prepared, width, { hanging: 0, opening: 0 });
      // An unbreakable URL or identifier must wrap natively rather than overflow.
      if (
        !Number.isFinite(layout.cost) ||
        !layout.lines.length ||
        layout.lines.some((line) => line.residual < -0.5)
      ) {
        setComposition(null);
        return;
      }
      const lines = layout.lines.map((line, index) => {
        const start = words[line.start].index + (line.startOffset ?? 0);
        const last = words[line.end - 1];
        const end = last.index + (line.endOffset ?? last[0].length);
        const next = layout.lines[index + 1];
        const nextStart = next ? words[next.start].index + (next.startOffset ?? 0) : text.length;
        return {
          text: text.slice(start, end),
          separator: text.slice(end, nextStart),
          wordSpacing: line.wordSpacing,
          tracking: line.tracking,
        };
      });
      setComposition({ text, lines });
    };
    const schedule = () => {
      // Nothing is composed while the box is moving; `rest` comes back for it.
      if (riding) return;
      cancelAnimationFrame(frame);
      frame = requestAnimationFrame(() => {
        // Measurement may be unavailable in an embedded or hidden document.
        try {
          fit();
        } catch {
          setComposition(null);
        }
      });
    };
    /** The box has come to rest: compose for the width it stopped at. */
    const rest = () => {
      riding = false;
      schedule();
    };
    /**
     * A paragraph that already stands composed goes back to NATIVE wrapping for as
     * long as its width keeps changing, and is composed again once it stops. One with
     * nothing composed yet — a paragraph still streaming in, or one arriving into a
     * column that just gained width — is composed straight away and never waits.
     */
    const resized = () => {
      // A selected passage keeps the composition it was selected in, moving or not.
      if (isSelected()) return;
      const width = parseFloat(getComputedStyle(element).width) || 0;
      const moved = width !== seenWidth;
      seenWidth = width;
      if (riding) {
        if (moved) {
          window.clearTimeout(settleTimer);
          settleTimer = window.setTimeout(rest, SETTLE_MS);
        }
        return;
      }
      if (moved && lastWidth > 0) {
        riding = true;
        lastWidth = 0;
        setComposition(null);
        settleTimer = window.setTimeout(rest, SETTLE_MS);
        return;
      }
      schedule();
    };
    const fontsChanged = () => {
      prepared = undefined;
      schedule();
    };
    const observer = new ResizeObserver(resized);
    observer.observe(element);
    window.addEventListener('resize', schedule);
    document.addEventListener('selectionchange', schedule);
    document.fonts?.addEventListener('loadingdone', fontsChanged);
    void document.fonts?.ready.then(() => {
      if (!disposed) fontsChanged();
    });
    void import('@kitlangton/justice')
      .then((loaded) => {
        if (disposed) return;
        engine = loaded;
        schedule();
      })
      .catch(() => {
        if (!disposed) setComposition(null);
      });
    return () => {
      disposed = true;
      cancelAnimationFrame(frame);
      window.clearTimeout(settleTimer);
      observer.disconnect();
      window.removeEventListener('resize', schedule);
      document.removeEventListener('selectionchange', schedule);
      document.fonts?.removeEventListener('loadingdone', fontsChanged);
    };
  }, [text]);

  const lines = composition?.text === text ? composition.lines : null;
  return (
    <Tag ref={ref} className={className} data-justice={lines ? '' : undefined}>
      {lines
        ? lines.map((line, index) => (
            <Fragment key={index}>
              <span
                style={{
                  display: 'inline-block',
                  width: '100%',
                  whiteSpace: 'nowrap',
                  textAlign: 'left',
                  hyphens: 'none',
                  fontKerning: 'none',
                  fontVariantLigatures: 'none',
                  wordSpacing: line.wordSpacing,
                  letterSpacing: line.tracking,
                }}
              >
                {line.text}
              </span>
              {line.separator}
            </Fragment>
          ))
        : children}
    </Tag>
  );
}
