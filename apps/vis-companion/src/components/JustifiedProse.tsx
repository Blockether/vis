import {
  Children,
  cloneElement,
  Fragment,
  isValidElement,
  useLayoutEffect,
  useMemo,
  useRef,
  useState,
  type CSSProperties,
  type ReactNode,
} from 'react';
import type { Prepared } from '@kitlangton/justice';
import { engine } from '../lib/justice';

type InlineProps = { children?: ReactNode; node?: { tagName?: string } };
type InlineContent = {
  children: ReactNode;
  text: string;
  measured: string;
  rich: boolean;
  code: boolean;
};
const INLINE_TAGS = new Set(['a', 'strong', 'em', 'del', 's', 'code', 'span', 'button']);
const LITERAL_STYLE: CSSProperties = { whiteSpace: 'pre', wordSpacing: 0, letterSpacing: 0 };

/** Only inline prose participates; block structure and hard breaks remain native. */
function inlineContent(children: ReactNode): InlineContent | null {
  let text = '';
  let measured = '';
  let rich = false;
  let code = false;
  const visit = (value: ReactNode, literal = false): boolean =>
    Children.toArray(value).every((part) => {
      if (typeof part === 'string' || typeof part === 'number') {
        const source = String(part);
        text += source;
        // Keep UTF-16 offsets identical while making code one indivisible token.
        measured += literal
          ? source.replace(/[ \t\r\n\f]/g, '\u00a0').replace(/[-‐]/g, '‑')
          : source;
        return true;
      }
      if (!isValidElement<InlineProps>(part)) return false;
      const tag = typeof part.type === 'string' ? part.type : part.props.node?.tagName;
      if (part.type !== Fragment && (!tag || !INLINE_TAGS.has(tag))) return false;
      rich ||= part.type !== Fragment;
      code ||= tag === 'code';
      return visit(part.props.children, literal || tag === 'code');
    });
  if (!visit(children)) return null;
  const words = measured.match(/[^ \t\r\n\f]+/g) ?? [];
  if (
    !text ||
    text !== text.trim() ||
    text.length > 16_384 ||
    /[\u00a0\u200e\u200f\u202a-\u202e\u2066-\u2069]/u.test(text) ||
    words.length > 400 ||
    (measured.match(/[-‐]/g)?.length ?? 0) > 128 ||
    words.some((word) => (word.match(/[-‐]/g)?.length ?? 0) > 16) ||
    [...text.matchAll(/\p{Letter}/gu)].some(
      ([letter]) => !/[\p{Script=Latin}\p{Script=Greek}\p{Script=Cyrillic}]/u.test(letter),
    )
  )
    return null;
  return { children, text, measured, rich, code };
}

/** Slice the React-owned inline tree, retaining links, styles and event handlers. */
function inlineSlice(
  content: InlineContent,
  start: number,
  end: number,
  spaceFont: CSSProperties,
): ReactNode {
  let offset = 0;
  const visit = (value: ReactNode, literal = false): ReactNode =>
    Children.map(value, (part) => {
      if (typeof part === 'string' || typeof part === 'number') {
        const source = String(part);
        const from = offset;
        offset += source.length;
        const text = source.slice(Math.max(0, start - from), Math.max(0, end - from));
        if (!content.rich || literal) return text;
        // Justice has one space advance. Match it even inside bold/emphasized links.
        return text.split(/([ \t\r\n\f]+)/g).map((piece, index) =>
          /^[ \t\r\n\f]+$/.test(piece) ? (
            <span key={index} style={spaceFont}>
              {piece}
            </span>
          ) : (
            piece
          ),
        );
      }
      if (!isValidElement<InlineProps>(part)) return null;
      const from = offset;
      const tag = typeof part.type === 'string' ? part.type : part.props.node?.tagName;
      const children = visit(part.props.children, literal || tag === 'code');
      if (offset <= start || from >= end) return null;
      const element = cloneElement(part, undefined, children);
      return tag === 'code' ? <span style={LITERAL_STYLE}>{element}</span> : element;
    });
  return visit(content.children);
}

/** Clone a source range for measurement, including partially selected ancestors. */
function measuredSlice(source: Node, start: number, end: number): Node[] {
  let offset = 0;
  const visit = (node: Node): Node | null => {
    const from = offset;
    if (node.nodeType === Node.TEXT_NODE) {
      const text = node.textContent ?? '';
      offset += text.length;
      return document.createTextNode(
        text.slice(Math.max(0, start - from), Math.max(0, end - from)),
      );
    }
    const copy = node.cloneNode(false);
    for (const child of node.childNodes) {
      const sliced = visit(child);
      if (sliced) copy.appendChild(sliced);
    }
    if (offset <= start || from >= end) return null;
    if (copy instanceof HTMLElement && copy.tagName === 'CODE')
      Object.assign(copy.style, LITERAL_STYLE);
    return copy;
  };
  return [...source.childNodes].map(visit).filter((node): node is Node => node !== null);
}

/** Justice owns segmentation and hyphen boundaries; the adapter supplies styled advances. */
function prepareInline(
  engine: typeof import('@kitlangton/justice'),
  content: InlineContent,
  source: Node,
  probe: HTMLElement,
): Prepared {
  // All placeholder advances are replaced before solving. A text-keyed measurement
  // cache would incorrectly give “word” and **word** the same width.
  const prepared = engine.prepare(content.measured, (text) => text.length);
  probe.textContent = ' ';
  prepared.space = probe.getBoundingClientRect().width;
  if (!(prepared.space > 0)) throw new RangeError('Space width must be positive');
  prepared.endHangs.fill(0);
  prepared.startHangs.fill(0);
  const cache = new Map<string, number>();
  // A Range reads ordinary styled tokens from one laid-out inline tree. Replacing
  // the probe for every word forces a full layout and style recalculation per token.
  probe.replaceChildren(...measuredSlice(source, 0, content.text.length));
  const range = document.createRange();
  const leaves: Text[] = [];
  const offsets = [0];
  const boxed: { node: Element; start: number; end: number }[] = [];
  const walker = document.createTreeWalker(probe, NodeFilter.SHOW_TEXT);
  while (walker.nextNode()) {
    const leaf = walker.currentNode as Text;
    if (!leaf.length) continue;
    leaves.push(leaf);
    const start = offsets[offsets.length - 1];
    offsets.push(start + leaf.length);
    // Code and controls have padding or borders. Measure those tokens as boxes.
    const box = leaf.parentElement?.closest('code, button');
    const last = boxed[boxed.length - 1];
    if (box && last?.node === box) last.end = start + leaf.length;
    else if (box) boxed.push({ node: box, start, end: start + leaf.length });
  }
  if (offsets[offsets.length - 1] !== content.text.length)
    throw new RangeError('Inline source mismatch');
  let boxProbe: HTMLElement | null = null;
  const locate = (index: number): readonly [Text, number] => {
    let low = 0;
    let high = leaves.length - 1;
    while (low < high) {
      const middle = Math.floor((low + high) / 2);
      if (offsets[middle + 1] <= index) low = middle + 1;
      else high = middle;
    }
    return [leaves[low], index - offsets[low]];
  };
  const measure = (start: number, end: number) => {
    const key = `${start}:${end}`;
    const cached = cache.get(key);
    if (cached !== undefined) return cached;
    let width: number;
    if (boxed.some((box) => start < box.end && end > box.start)) {
      // Keep the ranged tree untouched while preserving the full code/control box.
      // An out-of-flow child cannot affect its parent's inline measurement.
      if (!boxProbe) {
        boxProbe = probe.cloneNode(false) as HTMLElement;
        probe.appendChild(boxProbe);
      }
      boxProbe.replaceChildren(...measuredSlice(source, start, end));
      width = boxProbe.getBoundingClientRect().width;
    } else {
      const [startNode, startOffset] = locate(start);
      const [endNode, endOffset] = locate(end);
      range.setStart(startNode, startOffset);
      range.setEnd(endNode, endOffset);
      width = range.getBoundingClientRect().width;
    }
    if (!Number.isFinite(width) || width < 0) throw new RangeError('Invalid inline width');
    cache.set(key, width);
    return width;
  };
  const words = [...content.measured.matchAll(/[^ \t\r\n\f]+/g)];
  for (const [index, word] of words.entries()) {
    prepared.widths[index + 1] =
      prepared.widths[index] + measure(word.index, word.index + word[0].length);
    const fragments = prepared.hyphenation?.[index];
    if (!fragments) continue;
    const { offsets, widths, hyphenWidths } = fragments;
    for (let from = 0; from < offsets.length - 1; from++) {
      for (let to = from + 1; to < offsets.length; to++) {
        const at = from * offsets.length + to;
        widths[at] = measure(word.index + offsets[from], word.index + offsets[to]);
        // prepare() only supplies source-authored hyphens: no glyph is inserted.
        hyphenWidths[at] = widths[at];
      }
    }
  }
  return prepared;
}

type ProseLine = {
  start: number;
  end: number;
  separator: string;
  wordSpacing: number;
  tracking: number;
};
type Composition = { content: InlineContent; lines: ProseLine[]; spaceFont: CSSProperties };

/**
 * A width arrives as a BURST — the desk rail riding off its seam, a window dragged by
 * its corner — and the width a paragraph is composed for is its last frame. Composing
 * on each of the others solves and rewrites every line for a width nobody reads.
 */
const SETTLE_MS = 80;

const nearby = new Map<Element, () => void>();
let nearbyObserver: IntersectionObserver | null = null;

function disconnectNearbyObserver() {
  if (nearby.size) return;
  nearbyObserver?.disconnect();
  nearbyObserver = null;
}

/** One observer wakes only paragraphs approaching the viewport, not the entire transcript. */
function observeNearby(element: Element, compose: () => void): () => void {
  if (!nearbyObserver) {
    nearbyObserver = new IntersectionObserver(
      (entries) => {
        for (const entry of entries) {
          if (!entry.isIntersecting) continue;
          const wake = nearby.get(entry.target);
          if (!wake) continue;
          nearby.delete(entry.target);
          nearbyObserver?.unobserve(entry.target);
          wake();
        }
        disconnectNearbyObserver();
      },
      { rootMargin: `${window.innerHeight}px 0px` },
    );
  }
  nearby.set(element, compose);
  nearbyObserver.observe(element);
  return () => {
    nearby.delete(element);
    nearbyObserver?.unobserve(element);
    disconnectNearbyObserver();
  };
}

/** Compose opening prose before paint; React keeps ownership of all visible text. */
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
  const content = useMemo(() => (enabled ? inlineContent(children) : null), [children, enabled]);
  const [composition, setComposition] = useState<Composition | null>(null);

  useLayoutEffect(() => {
    const element = ref.current;
    if (
      !element ||
      !engine ||
      content === null ||
      typeof Intl.Segmenter !== 'function' ||
      typeof ResizeObserver !== 'function'
    )
      return;
    let disposed = false;
    let frame = 0;
    let prepared: Prepared | undefined;
    let fontKey = '';
    let lastWidth = 0;
    // Keep the opening viewport synchronous. Distant paragraphs remain native and
    // accessible until the shared observer reaches them ahead of scrolling.
    let bounds: DOMRect | null = null;
    if (typeof IntersectionObserver === 'function') {
      try {
        bounds = element.getBoundingClientRect();
      } catch {
        // Measurement is unavailable; compose() already falls back to native text.
      }
    }
    const margin = window.innerHeight;
    let active =
      !bounds ||
      !Number.isFinite(bounds.top) ||
      !Number.isFinite(bounds.bottom) ||
      (bounds.bottom >= -margin && bounds.top <= window.innerHeight + margin);
    /** Seed the initial width so the observer's first delivery is not a resize. */
    let seenWidth = active ? parseFloat(getComputedStyle(element).width) || 0 : 0;
    let settleTimer = 0;
    /** True while the box is moving and the paragraph is left to wrap natively. */
    let riding = false;
    const { text } = content;
    const words = [...content.measured.matchAll(/[^ \t\r\n\f]+/g)];
    // Snapshot only the original markup, never a previous composition's line spans.
    const source = element.cloneNode(true);
    if (source.textContent !== text) return;

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
          width: 'max-content',
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
          prepared = prepareInline(engine, content, source, probe);
          fontKey = nextFont;
        } finally {
          probe.remove();
        }
      }
      lastWidth = width;
      const layout = engine.solve(prepared, width, {
        hanging: 0,
        opening: 0,
        // Literal code does not inherit line tracking or stretched internal spaces.
        ...(content.code ? { tracking: 0 } : {}),
      });
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
          start,
          end,
          separator: text.slice(end, nextStart),
          wordSpacing: line.wordSpacing,
          tracking: line.tracking,
        };
      });
      setComposition({
        content,
        lines,
        spaceFont: {
          display: 'contents',
          fontFamily: style.fontFamily,
          fontSize: style.fontSize,
          fontWeight: style.fontWeight,
          fontStyle: style.fontStyle,
          fontStretch: style.fontStretch,
          fontFeatureSettings: style.fontFeatureSettings,
          fontVariationSettings: style.fontVariationSettings,
          fontOpticalSizing: style.fontOpticalSizing === 'none' ? 'none' : 'auto',
        },
      });
    };
    const compose = () => {
      // Measurement may be unavailable in an embedded or hidden document.
      try {
        fit();
      } catch {
        setComposition(null);
      }
    };
    const schedule = () => {
      // Nothing is composed while the box is moving or outside the preload range.
      if (riding || !active) return;
      cancelAnimationFrame(frame);
      frame = requestAnimationFrame(compose);
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
      if (!active) return;
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
    // Commit visible text before paint; distant text needs no composition yet.
    let stopObserving: (() => void) | undefined;
    if (active) compose();
    else {
      stopObserving = observeNearby(element, () => {
        active = true;
        seenWidth = parseFloat(getComputedStyle(element).width) || 0;
        compose();
      });
    }
    const observer = new ResizeObserver(resized);
    observer.observe(element);
    window.addEventListener('resize', schedule);
    document.addEventListener('selectionchange', schedule);
    const fontSet = document.fonts;
    fontSet?.addEventListener('loadingdone', fontsChanged);
    // A resolved ready promise does not mean the already-loaded face changed.
    if (fontSet && fontSet.status !== 'loaded') {
      void fontSet.ready.then(() => {
        if (!disposed) fontsChanged();
      });
    }
    return () => {
      disposed = true;
      cancelAnimationFrame(frame);
      window.clearTimeout(settleTimer);
      stopObserving?.();
      observer.disconnect();
      window.removeEventListener('resize', schedule);
      document.removeEventListener('selectionchange', schedule);
      fontSet?.removeEventListener('loadingdone', fontsChanged);
    };
  }, [content]);

  const lines = composition?.content === content ? composition?.lines : null;
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
                {inlineSlice(composition!.content, line.start, line.end, composition!.spaceFont)}
              </span>
              {line.separator}
            </Fragment>
          ))
        : children}
    </Tag>
  );
}
