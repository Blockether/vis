import {
  Children,
  cloneElement,
  Fragment,
  isValidElement,
  useLayoutEffect,
  useMemo,
  useRef,
  useState,
  type ReactNode,
} from 'react';
import { flushSync } from 'react-dom';

type InlineProps = { children?: ReactNode; node?: { tagName?: string } };
type InlineContent = {
  children: ReactNode;
  text: string;
  /** The text with the spaces inside atomic boxes made non-breaking, at the same offsets. */
  measured: string;
  /** Atomic boxes (code, buttons) as `[start, end)` text ranges. A line never ends inside one. */
  atomic: [start: number, end: number][];
  /**
   * Its lines can open wide gaps: it holds an atomic box, or a token that hyphenation
   * cannot split. Other prose keeps its native justified lines and costs no work at all.
   */
  risky: boolean;
  /**
   * The text and the markup that decides how it measures. A render that repeats it
   * keeps its lines, even though every child element is new.
   */
  signature: string;
};
const INLINE_TAGS = new Set(['a', 'strong', 'em', 'del', 's', 'code', 'span', 'button']);
const ATOMIC_TAGS = new Set(['code', 'button']);
/**
 * A token at least this long is a path, an identifier or a link. Hyphenation cannot split
 * it, so the line before it may have to stretch its few gaps across the whole column.
 */
const LONG_TOKEN = 18;
/**
 * Native justification stretches the gaps of every line but the last. Beside an atomic
 * box or a long token, a line can hold only one or two gaps. A line whose gaps would grow
 * wider than this many natural spaces keeps its natural spacing and ends ragged instead,
 * as the terminal client does.
 */
const MAX_GAP = 5;

/** Attributes that may change how inline markup measures; callbacks and parser nodes cannot. */
function shapeOf(props: object): string {
  let shape = '';
  for (const [key, value] of Object.entries(props)) {
    if (key === 'children') continue;
    if (key === 'style' || ['string', 'number', 'boolean'].includes(typeof value))
      shape += ` ${key}=${JSON.stringify(value)}`;
  }
  return shape;
}

/** Only inline prose participates; block structure and hard breaks remain native. */
function inlineContent(children: ReactNode): InlineContent | null {
  let text = '';
  let measured = '';
  let shape = '';
  let boxed = false;
  const atomic: [number, number][] = [];
  const visit = (value: ReactNode, literal = false): boolean =>
    Children.toArray(value).every((part) => {
      if (typeof part === 'string' || typeof part === 'number') {
        const source = String(part);
        text += source;
        shape += `#${source.length}`;
        measured += literal ? source.replace(/[ \t\r\n\f]/g, '\u00a0') : source;
        return true;
      }
      if (!isValidElement<InlineProps>(part)) return false;
      const tag = typeof part.type === 'string' ? part.type : part.props.node?.tagName;
      if (part.type !== Fragment && (!tag || !INLINE_TAGS.has(tag))) return false;
      const box = !literal && !!tag && ATOMIC_TAGS.has(tag);
      const start = text.length;
      shape += `<${tag ?? ''}${shapeOf(part.props)}>`;
      if (!visit(part.props.children, literal || box)) return false;
      shape += '</>';
      if (box) {
        boxed = true;
        atomic.push([start, text.length]);
      }
      return true;
    });
  if (!visit(children)) return null;
  const words = [...measured.matchAll(/[^ \t\r\n\f]+/g)];
  if (
    !text ||
    text !== text.trim() ||
    text.length > 16_384 ||
    /[\u00a0\u200e\u200f\u202a-\u202e\u2066-\u2069]/u.test(text) ||
    words.length > 400
  )
    return null;
  return {
    children,
    text,
    measured,
    atomic,
    risky: boxed || words.some(([word]) => word.length >= LONG_TOKEN),
    signature: `${shape}\u0000${text}`,
  };
}

/** Slice the React-owned inline tree, retaining links, styles and event handlers. */
function inlineSlice(content: InlineContent, start: number, end: number): ReactNode {
  let offset = 0;
  const visit = (value: ReactNode): ReactNode =>
    Children.map(value, (part) => {
      if (typeof part === 'string' || typeof part === 'number') {
        const source = String(part);
        const from = offset;
        offset += source.length;
        return source.slice(Math.max(0, start - from), Math.max(0, end - from));
      }
      if (!isValidElement<InlineProps>(part)) return null;
      const from = offset;
      const children = visit(part.props.children);
      if (offset <= start || from >= end) return null;
      return cloneElement(part, undefined, children);
    });
  return visit(content.children);
}

type ProseLine = {
  start: number;
  end: number;
  /** The source whitespace between this line and the next; empty after a split word. */
  separator: string;
  /** The browser hyphenated the word that this line ends in. */
  hyphen: boolean;
  /** Natural spacing: justified, its gaps would open wider than `MAX_GAP`. */
  ragged: boolean;
};

const spaces = new Map<string, number>();
let canvas: CanvasRenderingContext2D | null | undefined;

/**
 * One natural space in a paragraph's font. A canvas measures it without touching the
 * document, so the reads of many paragraphs share one layout.
 */
function spaceWidth(style: CSSStyleDeclaration): number {
  const font = `${style.fontStyle} ${style.fontWeight} ${style.fontSize} ${style.fontFamily}`;
  let width = spaces.get(font);
  if (width === undefined) {
    canvas ??= document.createElement('canvas').getContext('2d');
    if (!canvas) return 0;
    canvas.font = font;
    width = canvas.measureText(' ').width;
    spaces.set(font, width);
  }
  return width;
}

/**
 * The lines exactly as the browser broke them. Returns `null` when native justification
 * already reads well, or when a break cannot be kept outside the native flow.
 */
function nativeLines(element: HTMLElement, content: InlineContent): ProseLine[] | null {
  const style = getComputedStyle(element);
  if (
    style.direction !== 'ltr' ||
    style.writingMode !== 'horizontal-tb' ||
    style.whiteSpace !== 'normal' ||
    style.textTransform !== 'none' ||
    parseFloat(style.textIndent) !== 0
  )
    return null;
  const { text, measured } = content;
  const leaves: Text[] = [];
  const offsets = [0];
  const walker = document.createTreeWalker(element, NodeFilter.SHOW_TEXT);
  while (walker.nextNode()) {
    const leaf = walker.currentNode as Text;
    if (!leaf.length) continue;
    leaves.push(leaf);
    offsets.push(offsets[offsets.length - 1] + leaf.length);
  }
  if (offsets[offsets.length - 1] !== text.length) return null;
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
  const range = document.createRange();
  const rects = (start: number, end: number) => {
    const [startNode, startOffset] = locate(start);
    const [endNode, endOffset] = locate(end - 1);
    range.setStart(startNode, startOffset);
    range.setEnd(endNode, endOffset + 1);
    return [...range.getClientRects()].filter((rect) => rect.height > 0);
  };
  const middle = (rect: DOMRect) => rect.top + rect.height / 2;
  const inAtomic = (offset: number) =>
    content.atomic.some(([start, end]) => offset > start && offset < end);
  /**
   * Whether the browser drew a hyphen at a break inside a word: `false` when it breaks
   * without one, `null` when the break cannot be told apart and the lines stay native.
   */
  const hyphenated = (offset: number): boolean | null => {
    // Source hyphens and slashes are break opportunities without a glyph.
    if (/[-‐/]/u.test(text[offset - 1])) return false;
    const [leaf] = locate(offset);
    const owner = getComputedStyle(leaf.parentElement ?? element);
    if (
      owner.wordBreak === 'break-all' ||
      owner.overflowWrap === 'anywhere' ||
      owner.overflowWrap === 'break-word'
    )
      return false;
    const letters = /\p{L}/u.test(text[offset - 1]) && /\p{L}/u.test(text[offset]);
    return owner.hyphens === 'auto' && letters ? true : null;
  };
  const space = spaceWidth(style);
  if (!(space > 0)) return null;
  /** A gap that native justification stretched past `MAX_GAP` natural spaces. */
  const wide = (offset: number) => {
    if (offset < 0 || offset >= text.length || !/[ \t\r\n\f]/.test(measured[offset])) return false;
    const gap = rects(offset, offset + 1)[0];
    return !!gap && gap.width > MAX_GAP * space;
  };
  const words = [...measured.matchAll(/[^ \t\r\n\f]+/g)];

  // Each read costs tens of microseconds. Wide gaps open only beside a risky token: on
  // its own line, or on the line before it that the token did not fit. Read those first.
  const risky = [
    ...content.atomic,
    ...words
      .filter(([word]) => word.length >= LONG_TOKEN)
      .map(({ index, 0: word }): [number, number] => [index, index + word.length]),
  ];
  const suspects = new Set<number>();
  for (const [start, end] of risky) {
    suspects.add(end);
    let at = start - 1;
    suspects.add(at);
    while (at >= 0 && !/[ \t\r\n\f]/.test(measured[at])) at--;
    while (at >= 0 && /[ \t\r\n\f]/.test(measured[at])) at--;
    while (at >= 0 && !/[ \t\r\n\f]/.test(measured[at])) at--;
    suspects.add(at);
  }
  if (![...suspects].some(wide)) return null;

  /** The rectangles of each word, read once: one per line it stands on. */
  const read: DOMRect[][] = [];
  const wordRects = (index: number) => {
    let found = read[index];
    if (!found) {
      const { index: start, 0: word } = words[index];
      found = read[index] = rects(start, start + word.length);
      if (!found.length) throw new RangeError('Unmeasured word');
    }
    return found;
  };
  const lines: Omit<ProseLine, 'separator' | 'ragged'>[] = [];
  let lineStart = words[0].index;
  let lineEnd = lineStart;
  let bottom = Number.NaN;
  let next = 0;
  let guess = 4;
  /** The current line holds the rest of a split word and is not pushed yet. */
  let open = false;
  while (next < words.length) {
    if (Number.isNaN(bottom)) bottom = wordRects(next)[0].bottom;
    const onLine = (index: number) => middle(wordRects(index)[0]) <= bottom;
    if (!onLine(next)) {
      // The rest of a split word filled its line.
      lines.push({ start: lineStart, end: lineEnd, hyphen: false });
      lineStart = words[next].index;
      bottom = Number.NaN;
      open = false;
      continue;
    }
    // Gallop to the last word on this line, then search between the last two guesses.
    let low = next;
    let high = next;
    for (let step = guess; high < words.length - 1; step *= 2) {
      const probe = Math.min(words.length - 1, high + step);
      if (!onLine(probe)) {
        high = probe - 1;
        break;
      }
      low = high = probe;
    }
    while (low < high) {
      const at = Math.ceil((low + high) / 2);
      if (onLine(at)) low = at;
      else high = at - 1;
    }
    guess = Math.max(1, low - next + 1);
    const { index: start, 0: word } = words[low];
    const end = start + word.length;
    next = low + 1;
    lineEnd = end;
    const fragments = wordRects(low);
    if (middle(fragments[fragments.length - 1]) <= bottom) {
      lines.push({ start: lineStart, end, hyphen: false });
      if (next < words.length) lineStart = words[next].index;
      bottom = Number.NaN;
      open = false;
      continue;
    }
    open = true;
    // The browser split this word across lines: find each break by its first moved letter.
    let from = Math.max(start, lineStart);
    for (let guard = 0; guard < 64; guard++) {
      const last = rects(end - 1, end)[0];
      if (!last || middle(last) <= bottom) break;
      let first = from + 1;
      let past = end - 1;
      while (first < past) {
        const at = Math.floor((first + past) / 2);
        const glyph = rects(at, at + 1)[0];
        if (glyph && middle(glyph) > bottom) past = at;
        else first = at + 1;
      }
      if (inAtomic(first)) return null;
      const hyphen = hyphenated(first);
      if (hyphen === null) return null;
      lines.push({ start: lineStart, end: first, hyphen });
      const moved = rects(first, first + 1)[0];
      if (!moved) return null;
      lineStart = from = first;
      bottom = moved.bottom;
    }
  }
  if (open) lines.push({ start: lineStart, end: lineEnd, hyphen: false });

  let repaired = false;
  const composed = lines.map((line, index): ProseLine => {
    const next = lines[index + 1];
    let ragged = false;
    if (next) {
      const gap = measured.slice(line.start, line.end).search(/[ \t\r\n\f]/);
      ragged = gap >= 0 && wide(line.start + gap);
    }
    repaired ||= ragged;
    return {
      ...line,
      separator: next ? text.slice(line.end, next.start) : '',
      ragged,
    };
  });
  return repaired ? composed : null;
}

/**
 * Lines stay valid for every render that repeats the signature they were composed for.
 * Each composition mounts its own line nodes: Chrome can keep a space collapsed after
 * the text around it is rewritten in place, leaving a justified line short of its column.
 */
type Composition = { id: number; signature: string; lines: ProseLine[] };

/**
 * A width arrives as a BURST — the desk rail riding off its seam, a window dragged by
 * its corner. While it moves, native justified lines fit every frame; the lines are
 * read again once the width has rested this long.
 */
const SETTLE_MS = 80;

/**
 * Prose is checked while it is still this many scroller heights away, so its lines are
 * already in place when it scrolls into view.
 */
const LOOKAHEAD = 1;

/** Prose that is not on screen yet never takes more than this from one frame. */
const FRAME_BUDGET_MS = 6;

type Paragraph = {
  element: HTMLElement;
  /** The scroller whose view decides nearness, or `null` for the page. */
  root: Element | null;
  /** Within the look-ahead, as the placement or the observer last saw it. */
  near: boolean;
  /** Its width or fonts may have changed since its lines were last read. */
  stale: boolean;
  observed: boolean;
  disposed: boolean;
  /** False when its lines already stand for its current width and fonts. */
  due: () => boolean;
  /** Back to the native flow, so its lines can be read again. */
  revert: () => void;
  /** Read the native lines; the returned call commits them. */
  read: () => () => void;
  /** Its box changed size: `step` reads it again now, `ride` keeps native lines meanwhile. */
  resized: () => 'step' | 'ride' | null;
  fontsChanged: () => void;
  isSelected: () => boolean;
};

type View = { top: number; bottom: number };

const paragraphs = new Map<Element, Paragraph>();
/** Registered by the current commit, placed after all of its layout effects. */
const placing = new Set<Paragraph>();
/** Near and stale: read by `pump`, nearest to the reader first. */
const waiting = new Set<Paragraph>();
/** Left as they stand while a native selection covers them. */
const held = new Set<Paragraph>();
const scrollers = new WeakMap<Element, Element | null>();
const observers = new Map<Element | null, { observer: IntersectionObserver; size: number }>();
let fontSet: FontFaceSet | null = null;
let awaitedFonts: FontFaceSet | null = null;
let placementQueued = false;
let frame = 0;
let compositions = 0;
let sizes: ResizeObserver | null = null;

/** The nearest vertical scroller, cached per parent as the transcript's paint skip does. */
function scrollerOf(element: Element): Element | null {
  const parent = element.parentElement;
  if (!parent) return null;
  const known = scrollers.get(parent);
  if (known !== undefined) return known;
  let scroller: Element | null = null;
  for (let node: Element | null = parent; node; node = node.parentElement) {
    const overflow = getComputedStyle(node).overflowY;
    if (overflow === 'auto' || overflow === 'scroll') {
      scroller = node;
      break;
    }
  }
  scrollers.set(parent, scroller);
  return scroller;
}

/** A paragraph's gap to what the reader sees of its scroller (0 while any of it is inside). */
function distanceOf(paragraph: Paragraph, views: Map<Element | null, View>): [number, number] {
  // Without an observer nothing could wake a distant paragraph: treat all as visible.
  if (typeof IntersectionObserver !== 'function') return [0, 0];
  try {
    const rect = paragraph.element.getBoundingClientRect();
    let view = views.get(paragraph.root);
    if (!view) {
      const bounds = paragraph.root?.getBoundingClientRect();
      view = bounds
        ? { top: bounds.top, bottom: bounds.bottom }
        : { top: 0, bottom: window.innerHeight };
      views.set(paragraph.root, view);
    }
    if (![rect.top, rect.bottom, view.top, view.bottom].every(Number.isFinite)) return [0, 0];
    const reach = (view.bottom - view.top) * LOOKAHEAD;
    return [Math.max(0, view.top - rect.bottom, rect.top - view.bottom), reach];
  } catch {
    return [0, 0];
  }
}

/**
 * Read and commit the lines of each paragraph that needs it, nearest first. Lines keep
 * the native breaks, so nothing above, below or inside the paragraph moves vertically:
 * this can run at any time, also above the reader while a gesture owns the scroll. The
 * first `required` paragraphs are always read; the rest stop at `budget` milliseconds
 * and wait for the next frame.
 */
function compose(list: Paragraph[], required = list.length, budget = Number.POSITIVE_INFINITY) {
  const due = list.filter((paragraph) => {
    waiting.delete(paragraph);
    return !paragraph.disposed && paragraph.due();
  });
  if (!due.length) return;
  flushSync(() => {
    for (const paragraph of due) paragraph.revert();
  });
  // Reads change nothing: every paragraph here shares the one layout after the revert.
  const started = performance.now();
  const commits: (() => void)[] = [];
  for (const [index, paragraph] of due.entries()) {
    if (index >= required && commits.length && performance.now() - started > budget) {
      for (const later of due.slice(index)) request(later);
      break;
    }
    commits.push(paragraph.read());
  }
  flushSync(() => {
    for (const commit of commits) commit();
  });
}

/**
 * Runs once after each commit that mounts or changes prose, after every layout effect
 * and still before paint. Prose on screen is read here; `pump` reads the prose near it.
 */
function place() {
  placementQueued = false;
  const views = new Map<Element | null, View>();
  const visible: Paragraph[] = [];
  for (const paragraph of placing) {
    if (paragraph.disposed) continue;
    paragraph.root = scrollerOf(paragraph.element);
    observe(paragraph);
    const [distance, reach] = distanceOf(paragraph, views);
    paragraph.near = distance <= reach;
    if (distance === 0) visible.push(paragraph);
    else if (paragraph.near) waiting.add(paragraph);
  }
  placing.clear();
  compose(visible);
  if (waiting.size) schedulePump();
}

/** One observer per scroller follows every paragraph in and out of its look-ahead. */
function observe(paragraph: Paragraph) {
  if (paragraph.observed) return;
  if (typeof IntersectionObserver !== 'function') {
    paragraph.near = true;
    return;
  }
  let entry = observers.get(paragraph.root);
  if (!entry) {
    // Inside a scroller the page viewport cannot see ahead: the scroller clips its
    // children, so only the scroller itself can be the root of the look-ahead.
    const observer = new IntersectionObserver(nearby, {
      root: paragraph.root,
      rootMargin: `${LOOKAHEAD * 100}% 0px`,
    });
    entry = { observer, size: 0 };
    observers.set(paragraph.root, entry);
  }
  entry.size++;
  entry.observer.observe(paragraph.element);
  paragraph.observed = true;
}

function nearby(entries: IntersectionObserverEntry[]) {
  for (const entry of entries) {
    const paragraph = paragraphs.get(entry.target);
    if (!paragraph) continue;
    paragraph.near = entry.isIntersecting;
    if (paragraph.near && paragraph.stale) waiting.add(paragraph);
  }
  if (waiting.size) schedulePump();
}

/** Ask for a reading; a paragraph out of reach waits until it comes near. */
function request(paragraph: Paragraph) {
  if (paragraph.disposed) return;
  paragraph.stale = true;
  if (!paragraph.near) return;
  waiting.add(paragraph);
  schedulePump();
}

function schedulePump() {
  if (!frame) frame = requestAnimationFrame(pump);
}

/** Read waiting prose nearest to the reader first, within one frame's budget. */
function pump() {
  frame = 0;
  const views = new Map<Element | null, View>();
  const ready: [paragraph: Paragraph, distance: number][] = [];
  for (const paragraph of waiting) {
    if (paragraph.disposed || !paragraph.near || !paragraph.stale) {
      waiting.delete(paragraph);
      continue;
    }
    ready.push([paragraph, distanceOf(paragraph, views)[0]]);
  }
  ready.sort((a, b) => a[1] - b[1]);
  // Prose on screen is never deferred; the rest shares one frame's budget.
  const visible = ready.filter(([, distance]) => distance === 0).length;
  compose(
    ready.map(([paragraph]) => paragraph),
    visible,
    FRAME_BUDGET_MS,
  );
}

/**
 * One observer reports every paragraph that changed size in this frame, after layout and
 * before paint. Their lines are read together, on one layout.
 */
function resizedAll(entries: ResizeObserverEntry[]) {
  const steps: Paragraph[] = [];
  const rides: Paragraph[] = [];
  for (const entry of entries) {
    const paragraph = paragraphs.get(entry.target);
    const change = paragraph?.resized();
    if (change === 'step') steps.push(paragraph!);
    else if (change === 'ride') rides.push(paragraph!);
  }
  if (rides.length)
    flushSync(() => {
      for (const paragraph of rides) paragraph.revert();
    });
  compose(steps);
}

function windowResized() {
  for (const paragraph of paragraphs.values()) request(paragraph);
}

/** Only paragraphs a selection held are read again, once it no longer covers them. */
function selectionChanged() {
  for (const paragraph of held) {
    if (paragraph.isSelected()) continue;
    held.delete(paragraph);
    request(paragraph);
  }
}

function fontsLoaded() {
  for (const paragraph of paragraphs.values()) paragraph.fontsChanged();
}

function watchFonts() {
  const set = document.fonts ?? null;
  if (set !== fontSet) {
    fontSet?.removeEventListener('loadingdone', fontsLoaded);
    set?.addEventListener('loadingdone', fontsLoaded);
    fontSet = set;
  }
  // A resolved ready promise does not mean the already-loaded face changed.
  if (!set || set.status === 'loaded' || awaitedFonts === set) return;
  awaitedFonts = set;
  void set.ready.then(() => {
    if (awaitedFonts !== set) return;
    awaitedFonts = null;
    fontsLoaded();
  });
}

function register(paragraph: Paragraph): () => void {
  if (!paragraphs.size) {
    window.addEventListener('resize', windowResized);
    document.addEventListener('selectionchange', selectionChanged);
  }
  paragraphs.set(paragraph.element, paragraph);
  sizes ??= typeof ResizeObserver === 'function' ? new ResizeObserver(resizedAll) : null;
  sizes?.observe(paragraph.element);
  watchFonts();
  placing.add(paragraph);
  if (!placementQueued) {
    placementQueued = true;
    queueMicrotask(place);
  }
  return () => unregister(paragraph);
}

function unregister(paragraph: Paragraph) {
  paragraph.disposed = true;
  if (paragraphs.get(paragraph.element) === paragraph) paragraphs.delete(paragraph.element);
  placing.delete(paragraph);
  waiting.delete(paragraph);
  held.delete(paragraph);
  sizes?.unobserve(paragraph.element);
  const entry = paragraph.observed ? observers.get(paragraph.root) : undefined;
  if (entry) {
    entry.observer.unobserve(paragraph.element);
    if (--entry.size === 0) {
      entry.observer.disconnect();
      observers.delete(paragraph.root);
    }
  }
  if (paragraphs.size) return;
  window.removeEventListener('resize', windowResized);
  document.removeEventListener('selectionchange', selectionChanged);
  fontSet?.removeEventListener('loadingdone', fontsLoaded);
  fontSet = null;
  awaitedFonts = null;
  cancelAnimationFrame(frame);
  frame = 0;
  sizes?.disconnect();
  sizes = null;
}

/**
 * Justified prose that never reflows. The browser breaks and justifies every line, as
 * the `PROSE` classes ask. Only a line whose justified gaps would open wider than
 * `MAX_GAP` spaces keeps natural spacing, on the same break, the way the terminal client
 * justifies. React keeps ownership of all visible text.
 */
export function JustifiedProse({
  as: Tag = 'p',
  children,
  className,
  enabled = true,
}: {
  /** `span` serves phrasing-only parents, such as a button; give it a block class. */
  as?: 'p' | 'li' | 'span';
  children: ReactNode;
  className?: string;
  enabled?: boolean;
}) {
  const ref = useRef<HTMLParagraphElement & HTMLLIElement & HTMLSpanElement>(null);
  const content = useMemo(() => (enabled ? inlineContent(children) : null), [children, enabled]);
  const signature = content?.risky ? content.signature : null;
  const [composition, setComposition] = useState<Composition | null>(null);

  // Keyed by the signature: a parent render that repeats the same prose with new
  // elements keeps its lines instead of reading them again.
  useLayoutEffect(() => {
    const element = ref.current;
    if (!element || content === null || signature === null) return;
    /** Width and fonts the standing lines were read for; empty until the first read. */
    let readFor = '';
    let composed = false;
    let seenWidth = 0;
    let movedAt = Number.NEGATIVE_INFINITY;
    let riding = false;
    let settleTimer = 0;

    /** Replacing line nodes during a native selection would discard the range. */
    const isSelected = () => {
      const selection = window.getSelection();
      return !!selection && !selection.isCollapsed && selection.containsNode(element, true);
    };
    const shape = () => {
      const style = getComputedStyle(element);
      return [
        style.width,
        style.fontFamily,
        style.fontSize,
        style.fontWeight,
        style.fontStyle,
        style.fontStretch,
        style.fontFeatureSettings,
        style.fontVariationSettings,
        style.letterSpacing,
        style.wordSpacing,
      ].join('|');
    };
    const paragraph: Paragraph = {
      element,
      root: null,
      near: false,
      stale: true,
      observed: false,
      disposed: false,
      due: () => {
        paragraph.stale = false;
        if (riding) return false;
        if (isSelected()) {
          held.add(paragraph);
          return false;
        }
        return shape() !== readFor;
      },
      revert: () => {
        if (!composed) return;
        composed = false;
        setComposition(null);
      },
      read: () => {
        let lines: ProseLine[] | null = null;
        // Measurement may be unavailable in an embedded or hidden document.
        try {
          if (element.getBoundingClientRect().width > 0) {
            lines = nativeLines(element, content);
            readFor = shape();
          }
        } catch {
          lines = null;
        }
        if (!lines) return () => {};
        const id = ++compositions;
        const read = lines;
        return () => {
          composed = true;
          setComposition({ id, signature, lines: read });
        };
      },
      /**
       * A single step of the column is read again before it paints. During a burst the
       * paragraph rides on its native lines, which fit every frame, and is read again
       * where the width comes to rest.
       */
      resized: () => {
        const width = parseFloat(getComputedStyle(element).width) || 0;
        if (width === seenWidth) {
          // A delivery that keeps the width may still bring a new font size or style.
          request(paragraph);
          return null;
        }
        const first = !seenWidth;
        seenWidth = width;
        // The first delivery reports the width that the placement already read.
        if (first) return null;
        const now = performance.now();
        const burst = now - movedAt < SETTLE_MS;
        movedAt = now;
        if (isSelected()) {
          held.add(paragraph);
          return null;
        }
        if (burst || riding) {
          riding = true;
          window.clearTimeout(settleTimer);
          settleTimer = window.setTimeout(() => {
            riding = false;
            request(paragraph);
          }, SETTLE_MS);
          return 'ride';
        }
        paragraph.stale = true;
        return paragraph.near || composed ? 'step' : null;
      },
      fontsChanged: () => {
        readFor = '';
        request(paragraph);
      },
      isSelected,
    };
    const release = register(paragraph);
    return () => {
      window.clearTimeout(settleTimer);
      release();
      // The next signature reads native markup, even one that repeats an old one.
      setComposition(null);
    };
    // `content` changes whenever its signature does; the signature alone keys the work.
  }, [signature]);

  const lines = composition?.signature === signature ? composition?.lines : null;
  return (
    <Tag ref={ref} className={className} data-justified-lines={lines ? '' : undefined}>
      {lines && content
        ? lines.map((line, index) => (
            <Fragment key={`${composition!.id}:${index}`}>
              <span
                style={{
                  display: 'inline-block',
                  width: '100%',
                  whiteSpace: 'nowrap',
                  textAlignLast: line.ragged || index === lines.length - 1 ? 'start' : 'justify',
                }}
              >
                {inlineSlice(content, line.start, line.end)}
                {line.hyphen ? (
                  <span aria-hidden="true" className="select-none after:content-['-']" />
                ) : null}
              </span>
              {line.separator}
            </Fragment>
          ))
        : children}
    </Tag>
  );
}
