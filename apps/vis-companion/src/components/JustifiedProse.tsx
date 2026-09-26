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
import { flushSync } from 'react-dom';
import type { Prepared, WordFragments } from '@kitlangton/justice';
import { engine } from '../lib/justice';
import { readerOwnsScroll } from '../lib/reader-gesture';

type InlineProps = { children?: ReactNode; node?: { tagName?: string } };
type InlineContent = {
  children: ReactNode;
  text: string;
  measured: string;
  rich: boolean;
  code: boolean;
  /** Ascending offsets inside inline code where a line may end. */
  breaks: number[];
  /**
   * The text and the markup that decides how it measures. A render that repeats it
   * keeps its composition, even though every child element is new.
   */
  signature: string;
};
const INLINE_TAGS = new Set(['a', 'strong', 'em', 'del', 's', 'code', 'span', 'button']);
const LITERAL_STYLE: CSSProperties = { whiteSpace: 'pre', wordSpacing: 0, letterSpacing: 0 };
/**
 * Inline code may wrap where a reader expects it to: after its spaces, after a path
 * separator, or before a dot inside a name. Each match ends at the break. Hyphens and
 * every other character stay joined; earlier rules win when a word has too many breaks.
 */
const CODE_BREAKS = [
  /[^ \t\r\n\f][ \t\r\n\f]+(?=[^ \t\r\n\f])/g,
  /[^/ \t\r\n\f]\/(?=[^/ \t\r\n\f])/g,
  /[\p{L}\p{N}_)\]](?=\.[\p{L}_])/gu,
];
/**
 * Literal code never stretches, so a line of long code beside one or two words can only
 * be justified by opening holes between them. A line whose gaps would grow wider than
 * this many natural spaces keeps its natural spacing and ends ragged instead.
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
  let rich = false;
  let code = false;
  const candidates: [offset: number, rank: number][] = [];
  const visit = (value: ReactNode, literal = false): boolean =>
    Children.toArray(value).every((part) => {
      if (typeof part === 'string' || typeof part === 'number') {
        const source = String(part);
        if (literal)
          for (const [rank, pattern] of CODE_BREAKS.entries())
            for (const match of source.matchAll(pattern))
              candidates.push([text.length + match.index + match[0].length, rank]);
        text += source;
        shape += `#${source.length}`;
        // Keep UTF-16 offsets identical while joining code between its chosen breaks.
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
      shape += `<${tag ?? ''}${shapeOf(part.props)}>`;
      if (!visit(part.props.children, literal || tag === 'code')) return false;
      shape += '</>';
      return true;
    });
  if (!visit(children)) return null;
  const words = [...measured.matchAll(/[^ \t\r\n\f]+/g)];
  if (
    !text ||
    text !== text.trim() ||
    text.length > 16_384 ||
    /[\u00a0\u200e\u200f\u202a-\u202e\u2066-\u2069]/u.test(text) ||
    words.length > 400 ||
    (measured.match(/[-‐]/g)?.length ?? 0) > 128 ||
    words.some(([word]) => (word.match(/[-‐]/g)?.length ?? 0) > 16) ||
    [...text.matchAll(/\p{Letter}/gu)].some(
      ([letter]) => !/[\p{Script=Latin}\p{Script=Greek}\p{Script=Cyrillic}]/u.test(letter),
    )
  )
    return null;
  // Like source hyphens, code breaks stay bounded: at most 16 in a word and 128 in the
  // paragraph. Every fragment between two breaks is measured.
  const byRank = (a: readonly number[], b: readonly number[]) => a[1] - b[1] || a[0] - b[0];
  const chosen: [offset: number, rank: number][] = [];
  candidates.sort((a, b) => a[0] - b[0]);
  let next = 0;
  for (const word of words) {
    const end = word.index + word[0].length;
    const inside: [offset: number, rank: number][] = [];
    for (; next < candidates.length && candidates[next][0] < end; next++)
      inside.push(candidates[next]);
    chosen.push(...inside.sort(byRank).slice(0, 16));
  }
  const breaks = chosen
    .sort(byRank)
    .slice(0, 128)
    .map(([offset]) => offset)
    .sort((a, b) => a - b);
  return { children, text, measured, rich, code, breaks, signature: `${shape}\u0000${text}` };
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
  const widths = new Map<string, number>();
  const boxes = new Map<string, HTMLElement>();
  const request = (start: number, end: number) => {
    const key = `${start}:${end}`;
    if (widths.has(key) || boxes.has(key)) return key;
    if (boxed.some((box) => start < box.end && end > box.start)) {
      // Keep the ranged tree untouched while preserving the full code/control box.
      // An out-of-flow child cannot affect its parent's inline measurement.
      const box = probe.cloneNode(false) as HTMLElement;
      box.append(...measuredSlice(source, start, end));
      boxes.set(key, box);
    } else {
      const [startNode, startOffset] = locate(start);
      const [endNode, endOffset] = locate(end);
      range.setStart(startNode, startOffset);
      range.setEnd(endNode, endOffset);
      widths.set(key, range.getBoundingClientRect().width);
    }
    return key;
  };
  const words = [...content.measured.matchAll(/[^ \t\r\n\f]+/g)];
  let next = 0;
  const plans = words.map(
    (word, index): { start: number; whole: string; bounds: number[]; keys: string[][] } => {
      const start = word.index;
      const end = start + word[0].length;
      const whole = request(start, end);
      const bounds = new Set(prepared.hyphenation?.[index]?.offsets);
      for (; next < content.breaks.length && content.breaks[next] < end; next++)
        bounds.add(content.breaks[next] - start);
      if (!bounds.size) return { start, whole, bounds: [], keys: [] };
      // A line ending at a break inside code leaves the spaces before it between lines.
      const stop = (offset: number) => {
        let bound = start + offset;
        if (bound < end) while (content.measured[bound - 1] === '\u00a0') bound--;
        return bound;
      };
      const sorted = [...bounds.add(0).add(end - start)].sort((a, b) => a - b);
      const keys = sorted.map((from, i) =>
        sorted.map((to, j) => (j > i ? request(start + from, stop(to)) : '')),
      );
      return { start, whole, bounds: sorted, keys };
    },
  );
  // Lay every box out together: one layout pass instead of one per measured token.
  probe.append(...boxes.values());
  for (const [key, box] of boxes) widths.set(key, box.getBoundingClientRect().width);
  const width = (key: string) => {
    const value = widths.get(key) ?? Number.NaN;
    if (!Number.isFinite(value) || value < 0) throw new RangeError('Invalid inline width');
    return value;
  };
  const graphemes = new Intl.Segmenter(undefined, { granularity: 'grapheme' });
  const hyphenation = plans.map(({ start, whole, bounds, keys }, index) => {
    prepared.widths[index + 1] = prepared.widths[index] + width(whole);
    if (!bounds.length) return undefined;
    const size = bounds.length;
    const counts = bounds.map(
      (bound) => [...graphemes.segment(content.measured.slice(start, start + bound))].length,
    );
    const fragments: WordFragments = {
      offsets: bounds,
      widths: new Float64Array(size * size),
      hyphenWidths: new Float64Array(size * size),
      characters: new Float64Array(size * size),
      explicit: bounds.map((_, at) => at > 0 && at < size - 1),
    };
    for (let from = 0; from < size - 1; from++) {
      for (let to = from + 1; to < size; to++) {
        const at = from * size + to;
        // Source hyphens and code breaks are literal: no glyph is inserted.
        fragments.widths[at] = fragments.hyphenWidths[at] = width(keys[from][to]);
        fragments.characters[at] = counts[to] - counts[from];
      }
    }
    return fragments;
  });
  if (hyphenation.some(Boolean)) prepared.hyphenation = hyphenation;
  return prepared;
}

type ProseLine = {
  start: number;
  end: number;
  separator: string;
  wordSpacing: number;
  tracking: number;
};
/**
 * Lines stay valid for every render that repeats the signature they were composed for.
 * Each composition mounts its own line nodes: Chrome can keep a space collapsed after
 * the text around it is rewritten in place, leaving a justified line short of its column.
 */
type Composition = {
  id: number;
  signature: string;
  lines: ProseLine[];
  spaceFont: CSSProperties;
};

/**
 * A width arrives as a BURST — the desk rail riding off its seam, a window dragged by
 * its corner — and the width a paragraph is composed for is its last frame. Composing
 * on each of the others solves and rewrites every line for a width nobody reads.
 */
const SETTLE_MS = 80;

/**
 * Prose is composed while it is still this many scroller heights away, so it is
 * already justified when it scrolls into view. The transcript keeps the same
 * neighbourhood laid out; further prose stays native until the reader comes near.
 */
const LOOKAHEAD = 1;

/**
 * Composing can change a paragraph's height. Below the reader that moves nothing they
 * see; above them it moves every line on screen unless the scroll moves by the same
 * amount, and moving it while a gesture or its momentum owns the scroll would stop or
 * fight that gesture. Prose above the reader therefore waits until the scroller is this
 * quiet, and is then composed in place.
 */
const REST_MS = 150;

/** Prose that is not on screen yet never takes more than this from one frame. */
const FRAME_BUDGET_MS = 6;

type Paragraph = {
  element: HTMLElement;
  /** The scroller whose view decides nearness, or `null` for the page. */
  root: Element | null;
  /** Within the look-ahead, as the placement or the observer last saw it. */
  near: boolean;
  /** Its content, width or fonts changed since it was last composed. */
  stale: boolean;
  observed: boolean;
  disposed: boolean;
  compose: () => void;
  fontsChanged: () => void;
  isSelected: () => boolean;
};

type View = { top: number; bottom: number };
type Spot = { above: boolean; distance: number; reach: number; bottom: number };

const paragraphs = new Map<Element, Paragraph>();
/** Registered by the current commit, placed after all of its layout effects. */
const placing = new Set<Paragraph>();
/** Near and stale: composed by `pump`, nearest to the reader first. */
const waiting = new Set<Paragraph>();
/** Left as they stand while a native selection covers them. */
const held = new Set<Paragraph>();
const scrollers = new WeakMap<Element, Element | null>();
const observers = new Map<Element | null, { observer: IntersectionObserver; size: number }>();
let fontSet: FontFaceSet | null = null;
let awaitedFonts: FontFaceSet | null = null;
let placementQueued = false;
let frame = 0;
let restTimer = 0;
let lastScrollAt = Number.NEGATIVE_INFINITY;
/** The last pump left prose above the reader until the scroller rests. */
let holding = false;
let compositions = 0;

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

/**
 * Where a paragraph stands against what the reader sees of its scroller: its gap to
 * that view (0 while any of it is inside), whether it starts above the view, where a
 * new height would move everything below it, and where it ends.
 */
function locate(paragraph: Paragraph, views: Map<Element | null, View>): Spot {
  const visible = { above: false, distance: 0, reach: 0, bottom: 0 };
  // Without an observer nothing could wake a distant paragraph: treat all as visible.
  if (typeof IntersectionObserver !== 'function') return visible;
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
    if (![rect.top, rect.bottom, view.top, view.bottom].every(Number.isFinite)) return visible;
    const reach = (view.bottom - view.top) * LOOKAHEAD;
    const bottom = rect.bottom;
    if (rect.top < view.top)
      return { above: true, distance: Math.max(0, view.top - rect.bottom), reach, bottom };
    return { above: false, distance: Math.max(0, rect.top - view.bottom), reach, bottom };
  } catch {
    // Measurement is unavailable; composing falls back to native text.
    return visible;
  }
}

/**
 * Whether something else keeps the reader's lines in place in this scroller: the
 * browser's own scroll anchoring, or the screen that owns the scroller and says so
 * with `data-keeps-reading-position`. Moving that scroller here as well bills the
 * same height twice, and reads to its owner as a scroll nobody made.
 */
function anchorsItself(root: Element | null): boolean {
  if (root?.hasAttribute('data-keeps-reading-position')) return true;
  if (typeof CSS === 'undefined' || !CSS.supports?.('overflow-anchor', 'auto')) return false;
  const scroller = root ?? document.scrollingElement;
  return !!scroller && getComputedStyle(scroller).overflowAnchor !== 'none';
}

/**
 * Prose composed above the view moves everything below it by its change in height.
 * Scroll by the same amount, so the lines the reader sees stay where they were. The
 * lowest such paragraph carries every change above it in where its bottom now stands.
 */
function keepPlace(root: Element | null, paragraph: Paragraph, bottom: number) {
  if (anchorsItself(root)) return;
  try {
    const shift = paragraph.element.getBoundingClientRect().bottom - bottom;
    if (!shift || !Number.isFinite(shift)) return;
    if (root) root.scrollTop += shift;
    else window.scrollBy(0, shift);
  } catch {
    // Without a measurement the place cannot be kept; the composition still stands.
  }
}

function compose(list: Paragraph[]) {
  if (!list.length) return;
  // Commit every line before the browser paints the native text they replace.
  flushSync(() => {
    for (const paragraph of list) {
      waiting.delete(paragraph);
      paragraph.compose();
    }
  });
}

/**
 * Runs once after each commit that mounts or changes prose: after every layout effect,
 * including the one that scrolls a screen to where it opens, and still before paint.
 * Only prose that this paint shows is composed here; `pump` composes the rest near it.
 */
function place() {
  placementQueued = false;
  const views = new Map<Element | null, View>();
  const visible: Paragraph[] = [];
  for (const paragraph of placing) {
    if (paragraph.disposed) continue;
    paragraph.root = scrollerOf(paragraph.element);
    const spot = locate(paragraph, views);
    paragraph.near = spot.distance <= spot.reach;
    if (spot.distance === 0) visible.push(paragraph);
    else if (paragraph.near) waiting.add(paragraph);
    observe(paragraph);
  }
  placing.clear();
  compose(visible);
  if (waiting.size) schedulePump();
}

/** One observer per scroller follows every paragraph in and out of its look-ahead. */
function observe(paragraph: Paragraph) {
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

/** Ask for a composition; a paragraph out of reach waits until it comes near. */
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

/**
 * Compose waiting prose nearest to the reader first, within one frame's budget. Prose
 * on screen or below it is composed at once; prose above it waits for the scroller to
 * rest, and is then composed without moving the lines the reader sees.
 */
function pump() {
  frame = 0;
  window.clearTimeout(restTimer);
  const rest = !readerOwnsScroll() && performance.now() - lastScrollAt >= REST_MS;
  const views = new Map<Element | null, View>();
  const ready: [paragraph: Paragraph, spot: Spot][] = [];
  holding = false;
  for (const paragraph of waiting) {
    if (paragraph.disposed || !paragraph.near || !paragraph.stale) {
      waiting.delete(paragraph);
      continue;
    }
    const spot = locate(paragraph, views);
    if (spot.above && !rest) holding = true;
    else ready.push([paragraph, spot]);
  }
  ready.sort((a, b) => a[1].distance - b[1].distance);
  const started = performance.now();
  let taken = 0;
  /** Per scroller, the lowest paragraph composed above the view and where it ended. */
  const anchors = new Map<Element | null, [paragraph: Paragraph, bottom: number]>();
  if (ready.length) {
    flushSync(() => {
      for (const [paragraph, spot] of ready) {
        // Prose on screen is never deferred; the rest shares one frame's budget.
        if (taken && spot.distance > 0 && performance.now() - started > FRAME_BUDGET_MS) break;
        taken++;
        waiting.delete(paragraph);
        paragraph.compose();
        const lowest = anchors.get(paragraph.root);
        if (spot.above && (!lowest || spot.bottom > lowest[1]))
          anchors.set(paragraph.root, [paragraph, spot.bottom]);
      }
    });
  }
  for (const [root, [paragraph, bottom]] of anchors) keepPlace(root, paragraph, bottom);
  if (taken < ready.length) schedulePump();
  if (holding) restTimer = window.setTimeout(schedulePump, REST_MS);
}

function scrolled() {
  lastScrollAt = performance.now();
  // Prose held above the reader may have scrolled into view or below it meanwhile.
  if (holding) schedulePump();
}

function windowResized() {
  for (const paragraph of paragraphs.values()) request(paragraph);
}

/** Only paragraphs a selection held are refitted, once it no longer covers them. */
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
    window.addEventListener('scroll', scrolled, { capture: true, passive: true });
    document.addEventListener('selectionchange', selectionChanged);
  }
  paragraphs.set(paragraph.element, paragraph);
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
  window.removeEventListener('scroll', scrolled, { capture: true });
  document.removeEventListener('selectionchange', selectionChanged);
  fontSet?.removeEventListener('loadingdone', fontsLoaded);
  fontSet = null;
  awaitedFonts = null;
  cancelAnimationFrame(frame);
  frame = 0;
  window.clearTimeout(restTimer);
  holding = false;
}

/**
 * Justify inline prose while React keeps ownership of all visible text. Prose on screen
 * is composed before it is first painted, and prose near it before it scrolls into view.
 */
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
  const signature = content?.signature ?? null;
  const [composition, setComposition] = useState<Composition | null>(null);

  // Keyed by the signature: a parent render that repeats the same prose with new
  // elements keeps its lines and measurements instead of measuring everything again.
  useLayoutEffect(() => {
    const element = ref.current;
    if (
      !element ||
      !engine ||
      content === null ||
      signature === null ||
      typeof Intl.Segmenter !== 'function' ||
      typeof ResizeObserver !== 'function'
    )
      return;
    let prepared: Prepared | undefined;
    let fontKey = '';
    let lastWidth = 0;
    /** The width the box last stood at, so a delivery can tell whether it moved. */
    let seenWidth = 0;
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
      if (!engine) return;
      const style = getComputedStyle(element);
      // A later ResizeObserver delivery at the width fitted here is not a move.
      seenWidth = parseFloat(style.width) || 0;
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
      const loose = (MAX_GAP - 1) * prepared.space;
      const lines = layout.lines.map((line, index) => {
        const start = words[line.start].index + (line.startOffset ?? 0);
        const last = words[line.end - 1];
        let end = last.index + (line.endOffset ?? last[0].length);
        // A break after spaces inside code leaves them between the lines, outside the box.
        if (line.endOffset !== undefined) while (content.measured[end - 1] === '\u00a0') end--;
        const next = layout.lines[index + 1];
        const nextStart = next ? words[next.start].index + (next.startOffset ?? 0) : text.length;
        const ragged = line.wordSpacing > loose;
        return {
          start,
          end,
          separator: text.slice(end, nextStart),
          wordSpacing: ragged ? 0 : line.wordSpacing,
          tracking: ragged ? 0 : line.tracking,
        };
      });
      setComposition({
        id: ++compositions,
        signature,
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
    const paragraph: Paragraph = {
      element,
      root: null,
      near: false,
      stale: true,
      observed: false,
      disposed: false,
      compose: () => {
        // Nothing is composed while the box is moving; `rest` asks again once it stops.
        if (riding) return;
        paragraph.stale = false;
        if (isSelected()) {
          held.add(paragraph);
          return;
        }
        // Measurement may be unavailable in an embedded or hidden document.
        try {
          fit();
        } catch {
          setComposition(null);
        }
      },
      fontsChanged: () => {
        prepared = undefined;
        request(paragraph);
      },
      isSelected,
    };
    /** The box has come to rest: compose for the width it stopped at. */
    const rest = () => {
      riding = false;
      request(paragraph);
    };
    /**
     * A paragraph that already stands composed goes back to NATIVE wrapping for as
     * long as its width keeps changing, and is composed again once it stops. One with
     * nothing composed yet — a paragraph still streaming in, or one arriving into a
     * column that just gained width — is composed straight away and never waits.
     */
    const resized = () => {
      // A selected passage keeps the composition it was selected in, moving or not,
      // and is refitted once the selection no longer covers it.
      if (isSelected()) {
        held.add(paragraph);
        return;
      }
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
      // A delivery that keeps the width may still bring a new font size or style.
      request(paragraph);
    };
    const release = register(paragraph);
    const observer = new ResizeObserver(resized);
    observer.observe(element);
    return () => {
      window.clearTimeout(settleTimer);
      observer.disconnect();
      release();
      // The next signature snapshots native markup, even one that repeats an old one.
      setComposition(null);
    };
    // `content` changes whenever its signature does; the signature alone keys the work.
  }, [signature]);

  const lines = composition?.signature === signature ? composition?.lines : null;
  return (
    <Tag ref={ref} className={className} data-justice={lines ? '' : undefined}>
      {lines && content
        ? lines.map((line, index) => (
            <Fragment key={`${composition!.id}:${index}`}>
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
                {inlineSlice(content, line.start, line.end, composition!.spaceFont)}
              </span>
              {line.separator}
            </Fragment>
          ))
        : children}
    </Tag>
  );
}
