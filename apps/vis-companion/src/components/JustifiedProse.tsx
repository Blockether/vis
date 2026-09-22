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
    let engine: typeof import('@kitlangton/justice') | undefined;
    const words = [...text.matchAll(/[^ \t\r\n\f]+/g)];

    const fit = () => {
      if (disposed || !engine) return;
      // Replacing line nodes during a native selection would discard the range.
      // Refit once it is cleared instead of disrupting copying or annotation.
      const selection = window.getSelection();
      if (selection && !selection.isCollapsed && selection.containsNode(element, true)) return;
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
    const fontsChanged = () => {
      prepared = undefined;
      schedule();
    };
    const observer = new ResizeObserver(schedule);
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
