import { useEffect, useMemo, useState, useSyncExternalStore } from 'react';
import type { CSSProperties, RefObject } from 'react';
import { App } from '@capacitor/app';
import { Capacitor } from '@capacitor/core';
import type { PluginListenerHandle } from '@capacitor/core';
import { Keyboard } from '@capacitor/keyboard';
import type { KeyboardInfo } from '@capacitor/keyboard';

import {
  clampShellHeight,
  isKeyboardCovering,
  isViewportOversized,
  isViewportSettled,
  layoutHeight,
  readViewportMetrics,
} from './viewport-metrics';
import { hasHardwarePointer } from './pointer';

/** `input` types that never raise a keyboard. */
const NON_TEXT_INPUT_TYPES = new Set([
  'button',
  'checkbox',
  'color',
  'file',
  'image',
  'radio',
  'range',
  'reset',
  'submit',
]);

export function isKeyboardInputElement(element: Element | null): element is HTMLElement {
  if (!(element instanceof HTMLElement)) return false;
  const tag = element.tagName;
  return (
    tag === 'TEXTAREA' ||
    element.isContentEditable ||
    (tag === 'INPUT' && !NON_TEXT_INPUT_TYPES.has((element as HTMLInputElement).type.toLowerCase()))
  );
}

/** The keyboard editor that holds `target`, if any. */
function editorAt(target: Element): HTMLElement | null {
  for (let el: Element | null = target; el; el = el.parentElement) {
    if (isKeyboardInputElement(el)) {
      const disabled = (el as HTMLInputElement).disabled || (el as HTMLInputElement).readOnly;
      return disabled ? null : el;
    }
  }
  return null;
}

// iPadOS Safari reports a Mac platform; only its touch points tell it apart.
function isIosBrowser(): boolean {
  const nav = globalThis.navigator;
  if (!nav) return false;
  return /iP(?:hone|ad|od)/.test(nav.userAgent) || (nav.platform === 'MacIntel' && nav.maxTouchPoints > 1);
}

type KeyboardOrientation = 'portrait' | 'landscape';
type KeyboardHeightCache = Record<string, number>;

/**
 * The keyboard a field raises: the full `text` keyboard or the shorter number `pad`.
 * Reported on an iPhone: the composer taught the text height, and the one-time code
 * sheet then pinned for it. The pad rose lower, so the sheet dropped under the finger
 * and the second tap of a double tap (paste) missed the field.
 */
export type KeyboardKind = 'text' | 'pad';

const PAD_INPUT_MODES = new Set(['numeric', 'decimal', 'tel']);

/** The keyboard that a focus on `el` raises. */
export function keyboardKindOf(el: Element | null): KeyboardKind {
  if (!(el instanceof HTMLElement)) return 'text';
  const mode = el.getAttribute('inputmode')?.toLowerCase();
  if (mode) return PAD_INPUT_MODES.has(mode) ? 'pad' : 'text';
  return el instanceof HTMLInputElement && el.type === 'tel' ? 'pad' : 'text';
}

function activeKeyboardKind(): KeyboardKind {
  return keyboardKindOf(globalThis.document?.activeElement ?? null);
}

const KEYBOARD_HEIGHT_CACHE_KEY = 'vis.keyboard-heights.v1';

function keyboardOrientation(): KeyboardOrientation {
  return window.innerWidth > window.innerHeight ? 'landscape' : 'portrait';
}

function keyboardCacheSlot(
  orientation = keyboardOrientation(),
  kind: KeyboardKind = activeKeyboardKind(),
): string {
  const width = Math.round(window.screen.width || window.innerWidth);
  const height = Math.round(window.screen.height || window.innerHeight);
  const slot = `${Math.min(width, height)}x${Math.max(width, height)}:${orientation}`;
  return kind === 'text' ? slot : `${slot}:${kind}`;
}

// Capacitor's event reaches JavaScript after UIKit has already started moving.
// Keep the last native measurement synchronously in localStorage so even the
// first focus after a cold app launch can place the composer before that event.
function loadKeyboardHeightCache(): KeyboardHeightCache {
  try {
    const parsed: unknown = JSON.parse(
      globalThis.localStorage?.getItem(KEYBOARD_HEIGHT_CACHE_KEY) ?? '{}',
    );
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return {};
    return Object.fromEntries(
      Object.entries(parsed).filter(
        (entry): entry is [string, number] =>
          typeof entry[1] === 'number' && Number.isFinite(entry[1]) && entry[1] > 0,
      ),
    );
  } catch {
    return {};
  }
}

const keyboardHeightCache = loadKeyboardHeightCache();

// Standard docked iOS keyboards occupy a stable fraction of the physical window.
// This is only a one-frame-ahead prediction for a never-before-seen device/orientation:
// the native event corrects it, then the exact value replaces it in the cache.
const ESTIMATED_KEYBOARD_HEIGHT_RATIO: Record<KeyboardOrientation, number> = {
  portrait: 0.35,
  landscape: 0.41,
};

function isPlausibleKeyboardHeight(height: number, fullHeight: number): boolean {
  return Number.isFinite(height) && height >= fullHeight * 0.15 && height <= fullHeight * 0.7;
}

function keyboardHeightForPrepin(
  fullHeight: number,
  orientation = keyboardOrientation(),
  kind: KeyboardKind = activeKeyboardKind(),
): number {
  const cached = keyboardHeightCache[keyboardCacheSlot(orientation, kind)];
  if (cached && isPlausibleKeyboardHeight(cached, fullHeight)) return cached;
  return Math.round(fullHeight * ESTIMATED_KEYBOARD_HEIGHT_RATIO[orientation]);
}

function viewportOrientationSettled(): boolean {
  const vv = window.visualViewport;
  if (!vv) return true;
  const root = document.documentElement;
  const sizes = [
    [window.innerWidth, window.innerHeight],
    [vv.width, vv.height],
    [root.clientWidth, root.clientHeight],
  ] as const;
  // During an iOS flip the CSS root reaches the new physical device first,
  // while JS/visualViewport briefly describe the old device or become square.
  // Pin only once all three rulers have an aspect ratio and agree; unlike
  // `screen.orientation` / media queries, none of these waits for UIKit's event.
  if (sizes.some(([width, height]) => Math.abs(width - height) < 2)) return false;
  const landscape = sizes[0][0] > sizes[0][1];
  return sizes.every(([width, height]) => width > height === landscape);
}

function rememberKeyboardHeight(height: number): void {
  keyboardHeightCache[keyboardCacheSlot()] = height;
  try {
    globalThis.localStorage?.setItem(
      KEYBOARD_HEIGHT_CACHE_KEY,
      JSON.stringify(keyboardHeightCache),
    );
  } catch {
    // Private mode / quota: the in-memory cache still makes later focuses instant.
  }
}

/**
 * Re-measure schedule after a wake (backgrounded app, tab switch, bfcache
 * restore). iOS hands the webview stale `visualViewport` metrics for a few
 * frames after resume and fires no further resize/scroll, so one measurement is
 * not enough — settle over roughly a second and a half: a cold resume (the app
 * was suspended, not just backgrounded) keeps moving well past half a second.
 */
const WAKE_RESYNC_MS = [0, 60, 160, 320, 600, 900, 1400];

type Box = { height: number; top: number };

/**
 * Rotation is a LAYOUT event, and for a few hundred milliseconds the webview
 * describes two different devices at once: iOS keeps handing out the portrait
 * `visualViewport` height while the layout viewport is already landscape. Every
 * consumer that measures during that window records a box that never existed —
 * the shell pins itself to a stale height, the transcript reflows under a
 * scroll offset that no longer means anything, and the result is the visible
 * lurch-then-catch-up at the end of the rotation animation.
 *
 * So rotation gets its own signal instead of being inferred from `resize`:
 * `start` fires the moment the orientation flips (subscribers snapshot what
 * they must preserve), `settle` repeats while the OS finishes the animation,
 * and `end` closes the window. Nothing measures in between.
 */
export type RotationPhase = 'start' | 'settle' | 'end';

/**
 * The rotation window cannot be a fixed timeout. The OS animation is roughly
 * 300-500ms on iOS, the webview keeps re-laying out for a while after it, and
 * Android is done much sooner. A blind schedule therefore either closes while
 * the metrics are still moving — the shell re-pins to a box that is already
 * stale, which is the lurch — or long after they stopped, so the app keeps
 * re-measuring and visibly breathes. Watch the metrics instead and close the
 * window the frame they hold still.
 */
const ROTATION_MIN_MS = 220;
const ROTATION_MAX_MS = 1200;
const ROTATION_STABLE_FRAMES = 3;

const rotationListeners = new Set<(phase: RotationPhase) => void>();
let rotationFrame: number | null = null;
let rotationGuard: number | null = null;
let rotating = false;
let rotationWatched = false;

function emitRotation(phase: RotationPhase) {
  for (const listener of [...rotationListeners]) listener(phase);
}

/** Everything a consumer could measure, as one comparable string. */
function viewportSample(): string {
  const vv = window.visualViewport;
  const width = vv ? Math.round(vv.width) : 0;
  const height = vv ? Math.round(vv.height) : 0;
  return `${window.innerWidth}x${window.innerHeight}x${width}x${height}`;
}

/**
 * Undoes a leftover scroll of the LAYOUT viewport.
 *
 * iOS reveals a focused field by scrolling the layout viewport — it does that
 * even though `html` is `overflow: hidden` — and it does not undo it when the
 * app is suspended with the keyboard up. `position: fixed` resolves against
 * that layout viewport, so on resume the whole shell comes back shifted up by
 * the leftover offset: content rides under the status bar and the tab bar sits
 * below the fold, invisible and untappable, with no further viewport event to
 * correct it.
 */
function resetLayoutScroll(): void {
  const doc = document.scrollingElement ?? document.documentElement;
  if (window.scrollX !== 0 || window.scrollY !== 0) window.scrollTo(0, 0);
  if (doc.scrollTop !== 0) doc.scrollTop = 0;
  if (doc.scrollLeft !== 0) doc.scrollLeft = 0;
}

/** How far iOS has scrolled the LAYOUT viewport away from its origin. */
function layoutScroll(): number {
  const doc = document.scrollingElement ?? document.documentElement;
  return Math.max(0, window.scrollY || doc.scrollTop || 0);
}

function endRotation() {
  if (rotationFrame !== null) window.cancelAnimationFrame(rotationFrame);
  if (rotationGuard !== null) window.clearTimeout(rotationGuard);
  rotationFrame = null;
  rotationGuard = null;
  if (!rotating) return;
  rotating = false;
  emitRotation('end');
}

function watchSettle(startedAt: number, previous: string, held: number) {
  rotationFrame = window.requestAnimationFrame((now) => {
    rotationFrame = null;
    const sample = viewportSample();
    const stable = sample === previous ? held + 1 : 0;
    // Only a real geometry change is worth a settle: subscribers re-anchor on
    // it, and replaying that every frame is what made the content jitter.
    if (stable === 0) emitRotation('settle');
    const elapsed = now - startedAt;
    // "Held still" is not the same as "reflowed": iOS can leave the layout
    // viewport at its pre-rotation size for the first frames of a flip, and
    // those frames read as perfectly stable. So the window also waits until the
    // viewport fits the orientation the OS is actually in — otherwise it closes
    // before the reflow, and the reflow then lands with motion un-frozen and
    // the scroll anchor already spent, which is the jump. `ROTATION_MAX_MS`
    // still ends it unconditionally.
    const done =
      (stable >= ROTATION_STABLE_FRAMES &&
        elapsed >= ROTATION_MIN_MS &&
        isViewportSettled(readViewportMetrics())) ||
      elapsed >= ROTATION_MAX_MS;
    if (done) {
      endRotation();
      return;
    }
    watchSettle(startedAt, sample, stable);
  });
}

function beginRotation() {
  if (typeof window === 'undefined') return;
  if (rotationFrame !== null) window.cancelAnimationFrame(rotationFrame);
  if (rotationGuard !== null) window.clearTimeout(rotationGuard);
  rotationFrame = null;
  // `orientationchange` and the media query both fire for one physical
  // rotation; the second must extend the window, not restart the snapshot.
  if (!rotating) {
    rotating = true;
    // Do not stamp a state class onto <html>: a descendant-wide rotation rule
    // invalidates styles for every node in a transcript at both boundaries of
    // the flip. The rotation signal below already makes geometry consumers
    // suspend transitional measurements without touching the rendered tree.
    emitRotation('start');
  }
  // rAF stalls in a hidden webview, so the loop above could never close the
  // window and the shell would stay frozen. This always runs.
  rotationGuard = window.setTimeout(endRotation, ROTATION_MAX_MS + 200);
  watchSettle(performance.now(), viewportSample(), 0);
}

/**
 * Installed once for the app's lifetime. The media query is the reliable
 * signal everywhere (it flips exactly when layout does); `orientationchange`
 * is kept because older iOS fires it a frame earlier, and the duplicate is
 * absorbed above.
 */
function watchRotation() {
  if (rotationWatched || typeof window === 'undefined') return;
  rotationWatched = true;
  // Two web signals, no native help: the media query flips exactly when layout
  // does, and older iOS fires `orientationchange` a frame earlier. The duplicate
  // is absorbed in `beginRotation`.
  window.matchMedia?.('(orientation: portrait)').addEventListener('change', beginRotation);
  window.addEventListener('orientationchange', beginRotation);
}

/**
 * Scroll anchoring: keeping the reader's line while the page under them moves.
 *
 * Rotation rewraps every line, so the pixel `scrollTop` the reader parked at
 * stops pointing at what they were reading; "↑ Load earlier", the turn backfill
 * and a trace ramping its segments all insert content ABOVE them with the same
 * effect. The browser's own anchoring is not an option here (the transcript runs
 * `overflow-anchor: none` so prepending stays stable), so remember what is at
 * the fold and put it back.
 *
 * Two things measured on a 74-turn session decide the shape of this:
 *
 *   - the anchor must be the DEEPEST element crossing the fold, not a top-level
 *     child. One turn can be 40 000 px tall; while its segments hydrate, the row
 *     element's own `offsetTop` never moves, so a child-level anchor reports
 *     zero drift while the reader is shoved off the screen.
 *   - it is measured against the scroller's own top edge, in viewport
 *     coordinates, and applied as a RELATIVE correction. That is idempotent —
 *     whoever runs first in a frame fixes it, the next caller measures zero —
 *     which is what lets one observer own the whole job.
 */
type AnchorRect = { top: number; bottom: number };
type AnchorElement = {
  isConnected: boolean;
  getBoundingClientRect(): AnchorRect;
  children: ArrayLike<Element>;
  // `data-anchor="skip"` marks chrome that sits above the content it mutates —
  // the "load earlier" row — whose position says nothing about the reader.
  dataset?: { anchor?: string };
  nextElementSibling?: Element | null;
  querySelectorAll?(selectors: string): ArrayLike<Element>;
};
type AnchorScroller = {
  scrollTop: number;
  getBoundingClientRect(): AnchorRect;
  style?: { overflowY: string };
  readonly offsetHeight?: number;
};

/**
 * Sets `scrollTop` for a correction, also while iOS animates a flick.
 *
 * iOS WebKit runs momentum in the native scroll view. The next momentum frame
 * overwrites a script write. Measured on an iOS 26.5 iPhone: a 4 331 px anchor
 * correction was undone 40 ms later, and the reader landed near the start of
 * the turn. Hidden overflow stops the momentum, so the write stays. The flick
 * ends at the corrected line. Other platforms keep the plain write, because a
 * classic scrollbar changes the width when the overflow changes.
 */
export function seatScroller(viewport: AnchorScroller, top: number): void {
  const style = viewport.style;
  if (!style || !isIosBrowser()) {
    viewport.scrollTop = top;
    return;
  }
  const overflowY = style.overflowY;
  style.overflowY = 'hidden';
  viewport.scrollTop = top;
  // Lay out the hidden state, or the two style writes merge and the momentum continues.
  void viewport.offsetHeight;
  style.overflowY = overflowY;
}
/**
 * What the reader is looking at: the element crossing the fold and how far its
 * top sits below that fold — plus `top`, the `scrollTop` the reading was taken
 * at. That pixel is the part of the reading that OUTLIVES the element, and the
 * only thing left to put the reader back on when the turn they were reading is
 * unmounted and replaced.
 *
 * `below` holds the next sibling at each level of the walk, nearest first, with
 * its own offset. A commit can replace the anchored element while content lands
 * above it: a history page merges the first loaded step into an earlier segment
 * under a new key. The first connected sibling below still measures that growth.
 * An ancestor cannot: the history lands inside it, under its top edge.
 */
export type ScrollAnchor = {
  el: AnchorElement;
  offset: number;
  top: number;
  below: { el: AnchorElement; offset: number }[];
};

/** Deepest descendants win, but a transcript is deep: bound the walk. */
const ANCHOR_MAX_DEPTH = 24;

/**
 * First child whose bottom edge is still below the fold. Siblings are stacked,
 * so this is a binary search — it runs on every scroll event of a scroller with
 * hundreds of turns in it.
 */
function crossingChild(host: AnchorElement, target: number): AnchorElement | null {
  const children = host.children;
  let lo = 0;
  let hi = children.length - 1;
  while (lo <= hi) {
    const mid = (lo + hi) >> 1;
    const child = children[mid] as unknown as AnchorElement;
    if (child.getBoundingClientRect().bottom > target) hi = mid - 1;
    else lo = mid + 1;
  }
  for (let index = lo; index < children.length; index += 1) {
    const child = children[index] as unknown as AnchorElement;
    // Reading at the very top means the top-most child IS the "load earlier"
    // row, which is pinned above the history it loads: it measures zero drift
    // while 40 000 px lands underneath it. Take the first real turn instead.
    if (child.dataset?.anchor === 'skip' || !isInFlow(child)) continue;
    // An out-of-flow child can mislead the search, so check the crossing again.
    if (child.getBoundingClientRect().bottom > target) return child;
  }
  return null;
}

/**
 * True for a box in the normal flow: it stacks with its siblings and scrolls with
 * them. An `absolute`, `fixed` or `sticky` box can do neither. Measured on iOS
 * WebKit: an absolute `sr-only` label kept its top while the reader scrolled, so
 * as the anchor it billed each scroll step as growth. The reader jumped 12 px
 * against the finger, and the flick stopped.
 */
function isInFlow(node: AnchorElement): boolean {
  if (typeof getComputedStyle !== 'function') return true;
  const position = getComputedStyle(node as unknown as Element).position;
  return position !== 'absolute' && position !== 'fixed' && position !== 'sticky';
}

/** The next sibling in the normal flow, or null. */
function nextInFlow(node: AnchorElement): AnchorElement | null {
  let next = node.nextElementSibling as unknown as AnchorElement | null | undefined;
  while (next && !isInFlow(next)) {
    next = next.nextElementSibling as unknown as AnchorElement | null | undefined;
  }
  return next ?? null;
}

/**
 * The line that the anchor holds: the fold, or the bottom of a `skip` mark in the upper
 * half of the view. Content lands under such a mark. Held at the fold, it pushed down the
 * steps that the reader was reading: measured on iOS, an earlier-steps page replaced the
 * whole screen under the turn header. Held under the mark, it grows above the line, and
 * only the band above the mark changes. A mark in the lower half keeps the fold, because
 * most of what the reader sees is then above it.
 */
function anchorLine(view: AnchorRect, container: AnchorElement): number {
  let line = view.top;
  const middle = (view.top + view.bottom) / 2;
  const marks = container.querySelectorAll?.('[data-anchor="skip"]') ?? [];
  for (let index = 0; index < marks.length; index += 1) {
    const { top, bottom } = marks[index].getBoundingClientRect();
    if (top >= view.top && top < middle) line = Math.max(line, bottom);
  }
  return line;
}

/** What the reader is looking at, as an element, its offset from the fold and their own pixel. */
export function scrollAnchorFor(
  viewport: AnchorScroller,
  container: AnchorElement,
): ScrollAnchor | null {
  const view = viewport.getBoundingClientRect();
  const fold = view.top;
  const target = anchorLine(view, container);
  let node = crossingChild(container, target);
  if (!node) return null;
  const chain = [node];
  for (let depth = 0; depth < ANCHOR_MAX_DEPTH; depth += 1) {
    const deeper = crossingChild(node, target);
    if (!deeper) break;
    node = deeper;
    chain.push(node);
  }
  const below: ScrollAnchor['below'] = [];
  for (let index = chain.length - 1; index >= 0; index -= 1) {
    const next = nextInFlow(chain[index]);
    if (next) below.push({ el: next, offset: next.getBoundingClientRect().top - fold });
  }
  return {
    el: node,
    offset: node.getBoundingClientRect().top - fold,
    top: viewport.scrollTop,
    below,
  };
}

/** The anchored element, else the nearest connected sibling below it, else null. */
function anchorStillMounted(
  anchor: ScrollAnchor | null,
): { el: AnchorElement; offset: number } | null {
  if (!anchor) return null;
  if (anchor.el.isConnected) return anchor;
  return anchor.below.find((entry) => entry.el.isConnected) ?? null;
}

/**
 * Puts the anchored element back where it was. False means there was nothing to
 * restore (no anchor, or the turn and every sibling below it were unmounted
 * meanwhile) and the caller owns the fallback.
 */
export function applyScrollAnchor(viewport: AnchorScroller, anchor: ScrollAnchor | null): boolean {
  const held = anchorStillMounted(anchor);
  if (!anchor || !held) return false;
  const drift =
    held.el.getBoundingClientRect().top - viewport.getBoundingClientRect().top - held.offset;
  if (Math.abs(drift) > 0.5) moveAnchored(viewport, anchor, drift);
  return true;
}

/**
 * Moves the scroller by the growth above the anchor since it was taken, and by nothing
 * else. The reader's own scroll since then stays: the anchor's `top` takes it out.
 * False means there was no connected anchor to measure.
 */
export function applyScrollAnchorGrowth(
  viewport: AnchorScroller,
  anchor: ScrollAnchor | null,
): boolean {
  const held = anchorStillMounted(anchor);
  if (!anchor || !held) return false;
  const growth =
    held.el.getBoundingClientRect().top -
    viewport.getBoundingClientRect().top -
    held.offset +
    viewport.scrollTop -
    anchor.top;
  if (Math.abs(growth) > 0.5) moveAnchored(viewport, anchor, growth);
  return true;
}

/**
 * Moves the scroller by `delta` and the anchor's reading pixel with it, so the next
 * call measures zero. Without the second move, the reader's next scroll billed the
 * same growth again: an earlier-steps page of 1 351 px moved the reader twice.
 */
function moveAnchored(viewport: AnchorScroller, anchor: ScrollAnchor, delta: number): void {
  const before = viewport.scrollTop;
  seatScroller(viewport, Math.max(0, before + delta));
  anchor.top += viewport.scrollTop - before;
}

/** True while the viewport metrics are mid-rotation and must not be trusted. */
export function isViewportRotating(): boolean {
  return rotating;
}

/** Subscribe to rotation phases. Returns an unsubscribe. */
export function onViewportRotation(listener: (phase: RotationPhase) => void): () => void {
  watchRotation();
  rotationListeners.add(listener);
  return () => {
    rotationListeners.delete(listener);
  };
}

/**
 * True for the whole rotation window.
 *
 * The intermediate frames are real layout — two font sizes, two column widths,
 * a scroller that has not been re-anchored yet — so there is nothing to animate
 * between: any transition on them would interpolate boxes that never existed,
 * which is exactly the floating/resizing wobble. So consumers freeze motion for
 * the window (`[data-rotating]` in index.css) and let the settled layout simply
 * appear.
 *
 * Do NOT hide the shell for this window: the page background is the theme's
 * `--ink`, and the default theme is LIGHT, so fading the shell out paints a
 * full white screen for as long as the rotation takes.
 */
export function useIsViewportRotating(): boolean {
  const [isRotating, setRotating] = useState(false);
  useEffect(() => onViewportRotation((phase) => setRotating(phase !== 'end')), []);
  return isRotating;
}

/**
 * Pin the shell to iOS's visual viewport while its keyboard offsets the unchanged
 * layout viewport. Re-measure on every wake, clear styles when layouts match, and publish
 * a zero safe-bottom while the keyboard covers the home indicator.
 */
// Height currently forced onto the shell; native keyboard mode cannot be inferred from
// `visualViewport.height` alone.
let pinnedShellHeight: number | null = null;

// Whether the software keyboard is on screen, maintained by the native keyboard
// driver in `useVisualViewportShell` and read through `isSoftKeyboardUp`.
let softKeyboardUp = false;

// Whether the platform says the app is on screen, maintained by the native
// lifecycle driver in `useVisualViewportShell` and read through `isAppForeground`.
//
// It gates every programmatic focus change the app makes. A `blur()` or `focus()`
// issued from JS reaches the UI process one IPC hop later, where UIKit turns it
// into a keyboard task — and that queue only runs while the app is on screen. A
// task that lands during the background scene update parks the main thread on an
// `NSConditionLock` no keyboard thread is left to open: zero CPU, ten seconds,
// `0x8BADF00D` (TestFlight builds 4861 and 5275). `document.visibilityState`
// cannot answer this: a resumed iOS webview keeps reporting `hidden` (`lib/wake`).
let nativeForeground = true;

// `--safe-bottom` is deliberately NOT written on `document.documentElement`. A
// custom property set on the ROOT invalidates the computed style of every
// element that could inherit it, and the keyboard drivers below rewrite it on
// every focus, blur, pin, rotation and wake. Measured inside the app on a real
// transcript screen (~39k nodes) ONE root write cost 157 ms of style resolution
// and 256 ms once layout was forced — that was the freeze felt when the
// composer opened or closed. The value has two consumers, both footers, so it
// is published through this subscription and applied as an inline custom
// property on those elements: the same measurement put a leaf-scoped write at
// 0 ms.
const SAFE_BOTTOM_DEFAULT = 'env(safe-area-inset-bottom)';
const SAFE_BOTTOM_KEYBOARD = '0px';
let safeBottom: string = SAFE_BOTTOM_DEFAULT;
const safeBottomListeners = new Set<() => void>();

const setSafeBottom = (value: string): void => {
  if (safeBottom === value) return;
  safeBottom = value;
  for (const listener of safeBottomListeners) listener();
};

const subscribeSafeBottom = (listener: () => void): (() => void) => {
  safeBottomListeners.add(listener);
  return () => {
    safeBottomListeners.delete(listener);
  };
};

/**
 * Inline style carrying `--safe-bottom` for a footer that sits against the
 * bottom edge. Read it here instead of inheriting it from the document root:
 * the variable changes on every keyboard movement, and a root-scoped change is
 * a whole-document style recalculation.
 */
export function useSafeBottomStyle(): CSSProperties {
  const value = useSyncExternalStore(
    subscribeSafeBottom,
    () => safeBottom,
    () => SAFE_BOTTOM_DEFAULT,
  );
  return useMemo(() => ({ '--safe-bottom': value }) as CSSProperties, [value]);
}

// How much of the window's bottom edge the software keyboard is covering, in CSS
// pixels. The shell pin in `useVisualViewportShell` moves the app; it cannot move
// what is PORTALED out of it, and every menu and sheet is. On native iOS the
// webview never shrinks (`resize: 'none'`), so a sheet docked to the bottom sat
// under the keyboard with the field the human was typing into invisible. That
// sheet lifts itself by this value instead, published from the one place that
// already knows the geometry.
let keyboardInset = 0;
const keyboardInsetListeners = new Set<() => void>();

const setKeyboardInset = (value: number): void => {
  const next = Number.isFinite(value) ? Math.max(0, Math.round(value)) : 0;
  if (keyboardInset === next) return;
  keyboardInset = next;
  for (const listener of keyboardInsetListeners) listener();
};

const subscribeKeyboardInset = (listener: () => void): (() => void) => {
  keyboardInsetListeners.add(listener);
  return () => {
    keyboardInsetListeners.delete(listener);
  };
};

/**
 * Pixels of the bottom edge the software keyboard owns right now.
 *
 * `0` whenever the keyboard is down, and on the platforms that resize the webview
 * themselves — there the layout has already moved and lifting again would double
 * the gap. Only elements OUTSIDE the app shell need this; everything inside it is
 * moved by the shell pin.
 */
export function useKeyboardInset(): number {
  return useSyncExternalStore(
    subscribeKeyboardInset,
    () => keyboardInset,
    () => 0,
  );
}

// The band a sheet that holds a field keeps free below itself while the software
// keyboard is still down: the predicted keyboard height, less what the keyboard
// already covers. A focus shrinks the shell by that height in the same frame, so a
// sheet docked to the bottom jumped up under the finger. The second tap of a double
// tap then missed the field, focus dropped and the keyboard fell again.
// Each keyboard kind has its own band: the pad is shorter than the text keyboard.
const NO_KEYBOARD_RESERVE: Record<KeyboardKind, number> = { text: 0, pad: 0 };
let keyboardReserve = NO_KEYBOARD_RESERVE;
const keyboardReserveListeners = new Set<() => void>();

const setKeyboardReserve = (value: (kind: KeyboardKind) => number): void => {
  const band = (kind: KeyboardKind) => {
    const pixels = value(kind);
    return Number.isFinite(pixels) ? Math.max(0, Math.round(pixels)) : 0;
  };
  const next = { text: band('text'), pad: band('pad') };
  if (keyboardReserve.text === next.text && keyboardReserve.pad === next.pad) return;
  keyboardReserve = next;
  for (const listener of keyboardReserveListeners) listener();
};

const subscribeKeyboardReserve = (listener: () => void): (() => void) => {
  keyboardReserveListeners.add(listener);
  return () => {
    keyboardReserveListeners.delete(listener);
  };
};

/**
 * Pixels a sheet docked to the bottom keeps free for a keyboard that is not up yet.
 *
 * With this band under it, the sheet already stands where the keyboard puts it, so
 * a focus does not move the field. `kind` is the keyboard that the sheet's field
 * raises. `0` while the keyboard is up, with a hardware keyboard, and on every
 * platform except native iOS.
 */
export function useKeyboardReserve(kind: KeyboardKind = 'text'): number {
  return useSyncExternalStore(
    subscribeKeyboardReserve,
    () => keyboardReserve[kind],
    () => 0,
  );
}

/**
 * The band a sheet with a field of `kind` keeps below itself, or `0` without a field.
 *
 * The sheet keeps the place of its first prediction until it closes. Reported on an
 * iPhone: the first one-time code predicted a taller pad than the one that rose, and
 * the sheet dropped under the finger. Now a shorter keyboard leaves a band of sheet
 * paper above it. Only a taller keyboard still lifts the sheet.
 */
export function useKeyboardBand(kind: KeyboardKind | null): number {
  const reserve = useKeyboardReserve(kind ?? 'text');
  const inset = useKeyboardInset();
  const orientation = useSyncExternalStore(subscribeKeyboardReserve, keyboardOrientation, () => 'portrait');
  const key = kind === null ? null : `${kind}:${orientation}`;
  const [held, setHeld] = useState<{ key: string; height: number } | null>(null);
  // Hold the prediction while the keyboard is down. A new kind or orientation holds again.
  if (key !== null && inset === 0 && reserve > 0 && held?.key !== key) setHeld({ key, height: reserve });
  if (key === null) return 0;
  return held?.key === key ? Math.max(0, held.height - inset) : reserve;
}

/** The shell's current height in CSS pixels, keyboard pin included. */
export function shellViewportHeight(): number {
  const metrics = readViewportMetrics();
  if (pinnedShellHeight !== null) {
    // The CSS root is the first ruler to adopt a rotated device. Do not clamp a
    // new physical-root pin back through stale innerHeight/visualViewport data.
    return Math.max(0, Math.min(pinnedShellHeight, document.documentElement.clientHeight));
  }
  return clampShellHeight(metrics.visualHeight, metrics);
}

let reclaimShellForExternalNavigation: (() => void) | null = null;

/**
 * Drop keyboard geometry before a notification/deep-link navigation changes the
 * visible screen. iOS can suspend the WebView without delivering keyboard-hide;
 * focus and the old inline shell height then survive the resume even though the
 * software keyboard is gone.
 */
export function reclaimViewportForExternalNavigation(): void {
  const active = document.activeElement;
  // Foreground only: dropping focus from a backgrounded app is what the keyboard
  // watchdog kills for (see `nativeForeground`), and there is no keyboard left to
  // dismiss anyway.
  if (nativeForeground && isKeyboardInputElement(active)) active.blur();
  reclaimShellForExternalNavigation?.();
}

/**
 * Is the app on screen right now, as the native lifecycle reports it?
 *
 * The one gate on programmatic focus: iOS answers a JS `blur()`/`focus()` through
 * UIKit's keyboard task queue, which stops running the moment the app leaves the
 * foreground, and a task queued there holds the main thread until the watchdog
 * kills the process.
 */
export function isAppForeground(): boolean {
  return nativeForeground;
}

/**
 * Is the software keyboard on screen right now?
 *
 * `visualViewport` cannot answer on native iOS: `resize: 'none'` keeps WKWebView
 * full-height, so the visual viewport never shrinks and only Capacitor's
 * will-show/will-hide pair knows. Android resizes the webview itself and the web
 * build has no plugin, so both are measured. The native driver in
 * `useVisualViewportShell` owns `softKeyboardUp`.
 */
export function isSoftKeyboardUp(): boolean {
  if (Capacitor.getPlatform() === 'ios') return softKeyboardUp;
  return isKeyboardCovering(readViewportMetrics());
}

export function useVisualViewportShell(shellRef: RefObject<HTMLElement | null>): void {
  useEffect(() => {
    const shell = shellRef.current;
    const vv = window.visualViewport;
    if (!shell || !vv) return;

    // iOS only: Android resizes the webview itself, so `visualViewport` already
    // tells the whole truth there and a second driver would subtract the
    // keyboard twice.
    const nativeKeyboard = Capacitor.getPlatform() === 'ios';
    let currentBox: Box | null = null;
    const webKeyboard = !nativeKeyboard && isIosBrowser();
    // Geometry is latency-sensitive: a React state update costs a render and a
    // frame after focus. Mutate only this root's inline geometry synchronously;
    // none of its thousands of descendants has to render for a keyboard frame.
    const setBox = (next: Box | null) => {
      // What the shell gives up at the bottom is exactly what the keyboard took,
      // and portaled UI has to give up the same. `--safe-bottom` at 0 is this
      // driver's own word for "the keyboard owns the bottom edge", so a wake-time
      // oversize pin - a shorter shell with no keyboard - publishes nothing.
      const inset =
        next && safeBottom === SAFE_BOTTOM_KEYBOARD
          ? layoutHeight(readViewportMetrics()) - next.height - next.top
          : 0;
      setKeyboardInset(inset);
      setKeyboardReserve((kind) =>
        nativeKeyboard && !hasHardwarePointer()
          ? keyboardHeightForPrepin(layoutHeight(readViewportMetrics()), keyboardOrientation(), kind) - inset
          : 0,
      );
      if (
        currentBox === next ||
        (currentBox && next && currentBox.height === next.height && currentBox.top === next.top)
      )
        return;
      currentBox = next;
      if (!next) {
        shell.style.removeProperty('height');
        shell.style.removeProperty('transform');
        return;
      }
      shell.style.height = `${next.height}px`;
      if (next.top === 0) shell.style.removeProperty('transform');
      else shell.style.transform = `translateY(${next.top}px)`;
    };

    let frame = 0;
    // Set while the native driver owns the box.
    let keyboardPinned = false;
    // A pin rebuilt from the CSS root can already belong to the new physical
    // orientation while innerWidth/visualViewport still describe the old one.
    let keyboardPinOrientation: KeyboardOrientation | null = null;
    // What the keyboard is doing, independent of whether the shell is currently
    // pinned to it: a rotation has to drop the pin (the numbers behind it belong
    // to the other orientation) and then rebuild it from fresh ones. Module-level
    // so anything that has to know whether the keyboard is up (see
    // `isSoftKeyboardUp`) reads the same fact this driver maintains.
    softKeyboardUp = false;
    // UIKit emits synthetic hide/show pairs while rotating. Latch the confirmed
    // pre-rotation state before those events can erase it, then rebuild the pin
    // as soon as the new CSS/JS dimensions agree.
    let keyboardExpectedAfterRotation = false;
    let keyboardHeight = 0;
    // iOS rotation uniquely reports `didHide` before `willHide`. Hold that
    // out-of-order `did` briefly: its matching `will` can identify the synthetic
    // pair without delaying the normal will→did keyboard dismissal path.
    let outOfOrderDidHideTimer: number | null = null;
    // Assigned only by the native driver below; no-ops elsewhere.
    let repinKeyboard: () => boolean = () => false;
    let prepinKeyboard: (duringRotation?: boolean) => boolean = () => false;
    let timers: number[] = [];
    const clearTimers = () => {
      for (const t of timers) window.clearTimeout(t);
      timers = [];
    };

    const reclaimAfterExternalNavigation = () => {
      clearTimers();
      if (outOfOrderDidHideTimer !== null) {
        window.clearTimeout(outOfOrderDidHideTimer);
        outOfOrderDidHideTimer = null;
      }
      softKeyboardUp = false;
      keyboardExpectedAfterRotation = false;
      keyboardPinned = false;
      keyboardPinOrientation = null;
      pinnedShellHeight = null;
      setSafeBottom(SAFE_BOTTOM_DEFAULT);
      resetLayoutScroll();
      setBox(null);
    };
    reclaimShellForExternalNavigation = reclaimAfterExternalNavigation;

    // iOS Safari scrolls the page to reveal a field while its keyboard slides up.
    // The visual viewport reports that only after the slide, so the shell jumped
    // twice. A focus therefore shortens the shell by the predicted keyboard height
    // first: the field is already above the keys and Safari has nothing to reveal.
    let webPrepinHeight: number | null = null;
    let webFullHeight = 0;
    const prepinWebKeyboard = () => {
      if (!webKeyboard || hasHardwarePointer() || isViewportRotating()) return false;
      const metrics = readViewportMetrics();
      if (isKeyboardCovering(metrics)) return false;
      const fullHeight = layoutHeight(metrics);
      webFullHeight = fullHeight;
      webPrepinHeight = clampShellHeight(fullHeight - keyboardHeightForPrepin(fullHeight), metrics);
      pinnedShellHeight = webPrepinHeight;
      setSafeBottom(SAFE_BOTTOM_KEYBOARD);
      setBox({ height: webPrepinHeight, top: 0 });
      return true;
    };
    const sync = () => {
      cancelAnimationFrame(frame);
      frame = requestAnimationFrame(() => {
        // Mid-rotation iOS reports the PRE-rotation visual viewport against the
        // POST-rotation CSS root. That reads as a keyboard, and pinning the shell
        // to it fixes the app at a height the device no longer has — the visible
        // mismatch that used to correct itself several frames later as a jump.
        // Let the unpinned absolute shell follow the CSS root, except when the
        // physical-root fast path below already knows the new keyboard height.
        //
        // A keyboard pin is `fullHeight - keyboardHeight` measured in one
        // orientation; after the flip it is a height the device does not have,
        // and iOS does not reliably re-announce the keyboard early in rotation.
        // Only a screen with a focused field exposes this path, which is why the
        // session list could rotate cleanly while a transcript composer lagged.
        if (isViewportRotating()) {
          const root = document.documentElement;
          const physicalOrientation: KeyboardOrientation =
            root.clientWidth > root.clientHeight ? 'landscape' : 'portrait';
          // The CSS root is iOS's earliest trustworthy ruler. If the reversed
          // hide pair already rebuilt the pin from that new root, do not let a
          // later stale JS resize frame throw the exact geometry away again.
          if (
            keyboardPinned &&
            keyboardExpectedAfterRotation &&
            keyboardPinOrientation === physicalOrientation &&
            Math.abs(root.clientWidth - root.clientHeight) >= 2
          )
            return;
          keyboardPinned = false;
          resetLayoutScroll();
          setSafeBottom(SAFE_BOTTOM_DEFAULT);
          // The absolute shell follows the physical edge while WebKit's JS viewport
          // catches up. As soon as CSS and JS agree on the new orientation, restore
          // a still-open keyboard from the learned height (or a one-frame estimate)
          // instead of leaving the composer covered until rotation-end callbacks.
          if (prepinKeyboard(true)) return;
          // With no software keyboard, the unpinned absolute shell already
          // follows the CSS root in its first physical frame. Any JS-derived
          // height here belongs to the previous orientation and would shrink the
          // new screen until the late orientation event.
          pinnedShellHeight = null;
          setBox(null);
          return;
        }
        // The native keyboard driver owns the box while the keyboard is up: with
        // `resize: 'none'` the webview keeps its full size, so `visualViewport`
        // reports no keyboard at all and this would drop the pin mid-animation.
        if (keyboardPinned) return;
        // A suspended/hidden webview reports whatever it froze at; recording it
        // would outlive the state it described.
        if (document.visibilityState === 'hidden') return;
        const metrics = readViewportMetrics();
        // iOS Safari raises its keyboard only for a focused editor. Without one, a
        // short visual viewport is the keyboard still sliding down, not a keyboard.
        const covered =
          isKeyboardCovering(metrics) &&
          !(webKeyboard && !isKeyboardInputElement(document.activeElement));
        // A resume can hand back a layout viewport TALLER than the device (see
        // `deviceHeightLimit`), and iOS announces nothing afterwards. An
        // unpinned full-height shell would then hang off the bottom with the tab
        // bar and composer below the fold, so pin it to the physical device.
        const oversized = isViewportOversized(metrics);
        if (webPrepinHeight !== null) {
          // Hold the predicted pin until the keyboard covers the page or focus leaves.
          if (!covered && isKeyboardInputElement(document.activeElement)) return;
          webPrepinHeight = null;
        }
        // Safari shortens `innerHeight` while the keyboard is up, so measure the keys
        // against the height that the focus saw.
        if (webKeyboard && covered && webFullHeight > 0) {
          const keys = webFullHeight - vv.height;
          if (isPlausibleKeyboardHeight(keys, webFullHeight)) rememberKeyboardHeight(keys);
        }
        // Keyboard down means the layout viewport belongs at its origin again.
        if (!covered) resetLayoutScroll();
        // iOS can scroll the layout viewport to reveal a focused field even with
        // `html { overflow: hidden }`. A document-rooted shell moves with that
        // viewport, putting the header under the status bar, so pin it to the
        // visual viewport's document position. `pageTop` is that position in most
        // engines. iOS Safari updates `scrollY` in the scroll event but `pageTop`
        // up to 500 ms later (device trace), so it reads `scrollY` there.
        // `offsetTop + scrollY` counts the WebKit scroll twice.
        const top = Math.round(
          webKeyboard ? window.scrollY : (vv.pageTop ?? vv.offsetTop + layoutScroll()),
        );
        const next: Box | null =
          vv.height > 0 && (covered || oversized || top > 1)
            ? { height: clampShellHeight(vv.height, metrics), top }
            : null;
        pinnedShellHeight = next ? next.height : null;
        // Before the box: `setBox` reads this to publish the keyboard inset.
        setSafeBottom(covered ? SAFE_BOTTOM_KEYBOARD : SAFE_BOTTOM_DEFAULT);
        setBox(next);
      });
    };

    // Wake: measure now, then again as the OS settles the viewport.
    const resync = () => {
      clearTimers();
      // A wake is not a keyboard movement. iOS can suspend the webview between
      // `keyboardWillHide` and `keyboardDidHide`, and the missing `did` event
      // never arrives: `keyboardPinned` would stay set, `sync()` would keep
      // yielding to a keyboard driver that will never move again, and the shell
      // would stay frozen at a box the device no longer has. Release it here
      // unless the keyboard is genuinely still up.
      if (keyboardPinned && !softKeyboardUp && !isKeyboardInputElement(document.activeElement)) {
        keyboardPinned = false;
        pinnedShellHeight = null;
        setBox(null);
      }
      sync();
      for (const delay of WAKE_RESYNC_MS) timers.push(window.setTimeout(sync, delay));
    };

    const onVisible = () => {
      if (document.visibilityState === 'visible') resync();
    };

    // Focusing a field raises the keyboard a few frames later, and iOS reveals
    // that field by scrolling the *layout* viewport — which a shell pinned to the
    // visual viewport does not follow, so the caret can land off-screen. Move the
    // field only inside its own scroller, then put the document-rooted shell back.
    const revealFocusedEditor = (el: Element) => {
      if (document.activeElement !== el) return;
      el.scrollIntoView?.({ block: 'nearest' });
      // iOS Safari: the shell follows Safari's own reveal scroll (`pageTop`). A reset
      // here moved the page back while `pageTop` still reported the old scroll, which
      // left an empty band above the header for several frames (device trace).
      if (!webKeyboard) resetLayoutScroll();
      sync();
    };
    const onFocusIn = (event: FocusEvent) => {
      const el = event.target;
      if (!(el instanceof Element) || !isKeyboardInputElement(el)) return;
      // Capacitor's `keyboardWillShow` reaches this WebView after UIKit has
      // started moving. Once we know this orientation's keyboard height, pin in
      // the focus frame and let the event correct us if the layout changed. Native
      // scroll-to-reveal is disabled, so reveal the row against that shorter shell
      // in the same task; otherwise a field near the list's end stays behind the keys.
      if (nativeKeyboard) {
        prepinKeyboard();
        revealFocusedEditor(el);
        return;
      }
      const prepinned = prepinWebKeyboard();
      resync();
      timers.push(window.setTimeout(() => revealFocusedEditor(el), 350));
      // No keyboard came up (a hardware keyboard, or focus left early): drop the guess.
      if (prepinned)
        timers.push(
          window.setTimeout(() => {
            if (webPrepinHeight === null) return;
            webPrepinHeight = null;
            sync();
          }, 1000),
        );
    };

    // iOS Safari scrolls the page about 200 ms after the keyboard is up, even when
    // the field is already visible. Follow it in the scroll event itself: the next
    // animation frame is one painted frame late, and the header jumps up and back.
    const onPageScroll = () => {
      if (webKeyboard && currentBox)
        setBox({ height: currentBox.height, top: Math.round(window.scrollY) });
      sync();
    };

    // iOS Safari pans the page to reveal a field that a tap focuses, also when the
    // field is already above the keys. The pan is animated and script sees only its
    // end, so the header slid down and back (device and simulator traces). A tap
    // that focuses the field itself with `preventScroll` gets no pan. A drag stays
    // a scroll, and a tap on a focused field keeps its native caret placement.
    let tapStart: { x: number; y: number } | null = null;
    const onTouchStart = (event: TouchEvent) => {
      const touch = event.touches[0];
      tapStart = event.touches.length === 1 && touch ? { x: touch.clientX, y: touch.clientY } : null;
    };
    const onTouchEnd = (event: TouchEvent) => {
      const start = tapStart;
      tapStart = null;
      const touch = event.changedTouches[0];
      if (!start || !touch || Math.hypot(touch.clientX - start.x, touch.clientY - start.y) > 10) return;
      const el = event.target instanceof Element ? editorAt(event.target) : null;
      if (!el || el === document.activeElement || hasHardwarePointer()) return;
      event.preventDefault();
      el.focus({ preventScroll: true });
    };

    // iOS Safari: when the last editor loses focus, the keyboard slides down over the
    // page. Give the shell its full height at the origin at once. A shell that waits
    // for the late viewport events hangs 172 px down with a band below it (trace).
    const onFocusOut = () => {
      if (!webKeyboard) return;
      window.setTimeout(() => {
        if (isKeyboardInputElement(document.activeElement)) return;
        webPrepinHeight = null;
        pinnedShellHeight = null;
        resetLayoutScroll();
        setSafeBottom(SAFE_BOTTOM_DEFAULT);
        setBox(null);
      }, 0);
    };

    sync();
    vv.addEventListener('resize', sync);
    vv.addEventListener('scroll', sync);
    document.addEventListener('visibilitychange', onVisible);
    window.addEventListener('pageshow', resync);
    window.addEventListener('focus', onVisible);
    window.addEventListener('resize', sync);
    document.addEventListener('focusin', onFocusIn);
    document.addEventListener('focusout', onFocusOut);
    if (webKeyboard) {
      document.addEventListener('touchstart', onTouchStart, { capture: true, passive: true });
      document.addEventListener('touchend', onTouchEnd, { capture: true, passive: false });
    }
    // A layout-viewport scroll moves the document-rooted shell but fires no
    // visual-viewport event, so the browser fallback watches it explicitly. On
    // native iOS, Keyboard.setScroll and WKWebView scrolling are disabled; a
    // capture listener there would only intercept transcript scrolling at 60 Hz
    // and force useless viewport reads.
    if (!nativeKeyboard) window.addEventListener('scroll', onPageScroll, true);

    // Rotation, not a wake: `start` drops the stale pin in the frame the flip
    // happens, the settles re-check cheaply, and only the end pays for a full
    // wake schedule.
    const stopRotation = onViewportRotation((phase) => {
      if (phase !== 'end') {
        if (phase === 'start') {
          keyboardExpectedAfterRotation =
            softKeyboardUp && isKeyboardInputElement(document.activeElement);
        }
        sync();
        return;
      }
      // The rotation is over and both numbers finally describe the same device.
      // A keyboard that survived the flip owns the box again — re-pin it here,
      // because `sync()` cannot see a native keyboard at all (`resize: 'none'`)
      // and would leave the shell full-height with the composer behind it.
      const repinned = repinKeyboard();
      keyboardExpectedAfterRotation = false;
      if (repinned) return;
      resync();
    });

    // Native iOS keyboard: pin the full-height WebView's app shell directly.
    //
    // `visualViewport` arrives late and in coarse steps on iOS. Capacitor gives
    // us the final keyboard height earlier, but device measurements show that
    // its `keyboardWillShow` callback still arrives after UIKit starts moving.
    // Apply that height immediately—never add another web animation—and cache it
    // per orientation so later focuses can pre-pin in their first frame.
    //
    // `resize: 'none'` keeps WKWebView geometry stable while `setScroll` disables
    // native scroll-to-reveal. The absolute shell therefore has exactly one
    // geometry update and does not enter WebKit's lagging fixed-position layer.
    let disposeKeyboard = () => {};
    if (nativeKeyboard) {
      const subs: Promise<PluginListenerHandle>[] = [];
      const pin = (height: number, physicalLimit?: number) => {
        clearTimers();
        cancelAnimationFrame(frame);
        // Most callers use JS viewport metrics as the safety clamp. During a
        // rotation those metrics lag behind the already-correct CSS root, so the
        // root fast path passes that physical height as its authoritative limit.
        const nextHeight =
          physicalLimit === undefined
            ? clampShellHeight(height, readViewportMetrics())
            : Math.max(0, Math.min(Math.round(height), Math.round(physicalLimit)));
        const next = { height: nextHeight, top: 0 };
        const root = document.documentElement;
        keyboardPinOrientation = root.clientWidth > root.clientHeight ? 'landscape' : 'portrait';
        pinnedShellHeight = next.height;
        setBox(next);
      };
      const prepinKeyboardFromPhysicalRoot = () => {
        const root = document.documentElement;
        const rootWidth = root.clientWidth;
        const rootHeight = root.clientHeight;
        const innerLandscape = window.innerWidth > window.innerHeight;
        const rootLandscape = rootWidth > rootHeight;
        // During an iOS flip the CSS root reaches the new device before JS and
        // visualViewport. That disagreement is a trustworthy rotation signal
        // and lets us put the composer at its final edge in the first physical
        // layout frame, rather than waiting for any orientation/keyboard event.
        if (
          Math.abs(rootWidth - rootHeight) < 2 ||
          innerLandscape === rootLandscape ||
          !softKeyboardUp ||
          !isKeyboardInputElement(document.activeElement)
        )
          return false;
        keyboardExpectedAfterRotation = true;
        keyboardPinned = true;
        setSafeBottom(SAFE_BOTTOM_KEYBOARD);
        pin(
          rootHeight -
            keyboardHeightForPrepin(rootHeight, rootLandscape ? 'landscape' : 'portrait'),
          rootHeight,
        );
        return true;
      };
      prepinKeyboard = (duringRotation = false) => {
        if (
          duringRotation
            ? (!softKeyboardUp && !keyboardExpectedAfterRotation) || !viewportOrientationSettled()
            : softKeyboardUp || isViewportRotating()
        )
          return false;
        // A HARDWARE KEYBOARD RAISES NO KEYS. What focus brings up on a Magic
        // Keyboard iPad — or any desktop-class window — is at most a native
        // shortcut-bar callback, not the third of the screen this predicts. Neither
        // the estimate nor that callback may shorten the app shell. Rotation is
        // exempt: it only ever rebuilds the pin of a software keyboard already
        // confirmed up.
        if (!duringRotation && hasHardwarePointer()) return false;
        const metrics = readViewportMetrics();
        const fullHeight = layoutHeight(metrics);
        const predictedHeight = keyboardHeightForPrepin(fullHeight);
        keyboardPinned = true;
        setSafeBottom(SAFE_BOTTOM_KEYBOARD);
        pin(fullHeight - predictedHeight);
        if (duringRotation) return true;
        // A hardware keyboard can focus the field without ever raising the
        // software keyboard. Undo the speculative pin unless Capacitor confirms
        // it; `onWillShow` calls `pin`, which clears this timer.
        timers.push(
          window.setTimeout(() => {
            if (softKeyboardUp || !keyboardPinned) return;
            keyboardPinned = false;
            pinnedShellHeight = null;
            setBox(null);
            sync();
          }, 500),
        );
        return true;
      };
      const finishKeyboardHide = () => {
        keyboardPinned = false;
        pinnedShellHeight = null;
        setBox(null);
        sync();
      };
      const onWillShow = (info: KeyboardInfo) => {
        // iPadOS can announce either the hidden shortcut bar's tiny frame or the
        // whole software-keyboard frame parked below a hardware keyboard. A fine
        // pointer is the evidence that physical keys are attached; neither frame
        // covers this desktop-class app, regardless of the height UIKit reports.
        if (hasHardwarePointer()) {
          softKeyboardUp = false;
          keyboardHeight = 0;
          keyboardExpectedAfterRotation = false;
          setSafeBottom(SAFE_BOTTOM_DEFAULT);
          finishKeyboardHide();
          return;
        }
        softKeyboardUp = true;
        keyboardHeight = info.keyboardHeight;
        // Mid-rotation the two operands describe different devices — iOS reports
        // the new keyboard height while `innerHeight` is still the old window,
        // or the reverse. Record the height and let the rotation's `end` do the
        // arithmetic once they agree.
        if (isViewportRotating()) {
          keyboardExpectedAfterRotation = true;
          return;
        }
        const fullHeight = layoutHeight(readViewportMetrics());
        if (isPlausibleKeyboardHeight(keyboardHeight, fullHeight))
          rememberKeyboardHeight(keyboardHeight);
        keyboardPinned = true;
        setSafeBottom(SAFE_BOTTOM_KEYBOARD);
        pin(fullHeight - keyboardHeight);
      };
      const onWillHide = () => {
        // Rotation on iOS 26 reports `didHide` before `willHide`, before either
        // web orientation signal. A hardware-keyboard attachment can report the
        // same reversed pair while leaving the field focused, so the order alone
        // is not proof of rotation. Preserve the pin briefly, then require an
        // actual rotation signal; otherwise reclaim the full shell.
        if (outOfOrderDidHideTimer !== null && isKeyboardInputElement(document.activeElement)) {
          window.clearTimeout(outOfOrderDidHideTimer);
          outOfOrderDidHideTimer = window.setTimeout(() => {
            outOfOrderDidHideTimer = null;
            // The CSS root can expose the flip before matchMedia/orientationchange.
            if (prepinKeyboardFromPhysicalRoot()) return;
            if (isViewportRotating() || keyboardExpectedAfterRotation) return;
            softKeyboardUp = false;
            setSafeBottom(SAFE_BOTTOM_DEFAULT);
            finishKeyboardHide();
          }, 250);
          return;
        }
        if (isViewportRotating() || keyboardExpectedAfterRotation) return;
        softKeyboardUp = false;
        setSafeBottom(SAFE_BOTTOM_DEFAULT);
        pin(layoutHeight(readViewportMetrics()));
      };
      // Rebuild the keyboard pin from post-rotation numbers. Snaps rather than
      // transitions: the rotation already moved everything at once.
      repinKeyboard = () => {
        if (
          (!softKeyboardUp && !keyboardExpectedAfterRotation) ||
          !isKeyboardInputElement(document.activeElement)
        )
          return false;
        const fullHeight = layoutHeight(readViewportMetrics());
        const measuredHeight = isPlausibleKeyboardHeight(keyboardHeight, fullHeight)
          ? keyboardHeight
          : null;
        if (measuredHeight !== null) rememberKeyboardHeight(measuredHeight);
        keyboardPinned = true;
        setSafeBottom(SAFE_BOTTOM_KEYBOARD);
        pin(fullHeight - (measuredHeight ?? keyboardHeightForPrepin(fullHeight)));
        return true;
      };
      const onDidHide = () => {
        // This must run before the generic rotation guard: ResizeObserver can mark
        // the rotation in the same task that changes the CSS root, before this
        // native callback. The root/inner disagreement is precisely the signal
        // that makes this early pin safe.
        if (prepinKeyboardFromPhysicalRoot()) return;
        if (isViewportRotating() || keyboardExpectedAfterRotation) return;
        // A genuine will→did dismissal already cleared `softKeyboardUp` in
        // `onWillHide`; seeing it still set means this may be iOS's reversed
        // rotation pair, so hold briefly for the matching will event.
        if (softKeyboardUp && isKeyboardInputElement(document.activeElement)) {
          if (outOfOrderDidHideTimer !== null) window.clearTimeout(outOfOrderDidHideTimer);
          outOfOrderDidHideTimer = window.setTimeout(() => {
            outOfOrderDidHideTimer = null;
            if (isViewportRotating() || keyboardExpectedAfterRotation) return;
            softKeyboardUp = false;
            finishKeyboardHide();
          }, 40);
          return;
        }
        finishKeyboardHide();
      };
      try {
        subs.push(
          Keyboard.addListener('keyboardWillShow', onWillShow),
          Keyboard.addListener('keyboardWillHide', onWillHide),
          Keyboard.addListener('keyboardDidHide', onDidHide),
        );
        void Keyboard.setScroll({ isDisabled: true }).catch(() => undefined);
        // THE GRAY BAR OVER THE COMPOSER. UIKit hangs a form accessory bar —
        // field arrows and a Done key — off every focused web field. When a
        // hardware keyboard is attached (an iPad with a keyboard, and this app
        // running on a Mac) UIKit draws no keyboard, so that bar is the only
        // thing that arrives: a ~36px neutral panel, inset from the window
        // edges, in none of the app's colours, laid over the composer for as
        // long as you type. A screen here owns ONE field, so its field-to-field
        // arrows navigate nothing, and `Done` repeats the composer's own send.
        void Keyboard.setAccessoryBarVisible({ isVisible: false }).catch(() => undefined);
      } catch {
        /* plugin unavailable */
      }
      disposeKeyboard = () => {
        if (outOfOrderDidHideTimer !== null) window.clearTimeout(outOfOrderDidHideTimer);
        for (const sub of subs) void sub.then((handle) => handle.remove()).catch(() => undefined);
      };
    }

    // Native background: `appStateChange` reaches JS from
    // `applicationWillResignActive`, where the AppDelegate has already called
    // `endEditing(true)` — UIKit's own first responder release, taken in the UI
    // process while the keyboard machinery still answers. Leaving the app ends
    // editing: the keyboard is on its way down and does not come back.
    //
    // What must NOT happen here is a DOM `blur()`. It reaches the UI process as
    // `didProgrammaticallyClearFocusedElement` one IPC hop later, with the scene
    // update to Background already under way; UIKit turns it into a synchronous
    // keyboard task no thread is left to run, and the main thread waits on an
    // `NSConditionLock` until the watchdog kills Vis — zero CPU, ten seconds,
    // `0x8BADF00D` (TestFlight build 5275, and 4861 before it from the resume
    // side). Only the keyboard geometry goes now: the shell is back at full
    // height before the app is, so the return shows no band and no jump.
    const onNativeBackground = () => {
      nativeForeground = false;
      if (nativeKeyboard) reclaimAfterExternalNavigation();
    };

    // Act only after appStateChange reports active (didBecomeActive on iOS).
    // Capacitor's resume is willEnterForeground: touching DOM focus there can
    // block UIKit's keyboard queue before the app is active (builds 5358/5468).
    //
    // The return never raises the keyboard. A programmatic focus there pinned the
    // shell for a keyboard iOS then refused, so the composer jumped up and back.
    // WebKit keeps the DOM focus that `endEditing` left behind, and a tap on an
    // already-focused field raises nothing, so drop that focus now that UIKit
    // answers again. The next tap then opens the keyboard as usual.
    const onNativeResume = () => {
      nativeForeground = true;
      if (
        nativeKeyboard &&
        (keyboardPinned || softKeyboardUp || isKeyboardInputElement(document.activeElement))
      )
        reclaimViewportForExternalNavigation();
      resync();
    };
    let disposeNative = () => {};
    try {
      const handles: Array<{ remove: () => void }> = [];
      let removed = false;
      const track = (sub: { remove: () => void }) => {
        if (removed) sub.remove();
        else handles.push(sub);
      };
      void App.addListener('appStateChange', ({ isActive }) => {
        if (isActive) onNativeResume();
        else onNativeBackground();
      })
        .then(track)
        .catch(() => undefined);
      disposeNative = () => {
        removed = true;
        for (const sub of handles.splice(0)) sub.remove();
      };
    } catch {
      /* plugin unavailable */
    }

    return () => {
      cancelAnimationFrame(frame);
      clearTimers();
      if (reclaimShellForExternalNavigation === reclaimAfterExternalNavigation)
        reclaimShellForExternalNavigation = null;
      vv.removeEventListener('resize', sync);
      vv.removeEventListener('scroll', sync);
      document.removeEventListener('visibilitychange', onVisible);
      window.removeEventListener('pageshow', resync);
      window.removeEventListener('focus', onVisible);
      window.removeEventListener('resize', sync);
      document.removeEventListener('focusin', onFocusIn);
      document.removeEventListener('focusout', onFocusOut);
      document.removeEventListener('touchstart', onTouchStart, true);
      document.removeEventListener('touchend', onTouchEnd, true);
      window.removeEventListener('scroll', onPageScroll, true);
      stopRotation();
      disposeNative();
      disposeKeyboard();
      pinnedShellHeight = null;
      setBox(null);
      setKeyboardReserve(() => 0);
    };
  }, [shellRef]);
}
