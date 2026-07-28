import { useEffect, useState } from 'react';
import type { CSSProperties } from 'react';
import { App } from '@capacitor/app';
import { Capacitor } from '@capacitor/core';
import type { PluginListenerHandle } from '@capacitor/core';
import { Keyboard } from '@capacitor/keyboard';
import type { KeyboardInfo } from '@capacitor/keyboard';

import { installNativeViewport, onNativeViewport } from './native-viewport';
import {
  clampShellHeight,
  deviceHeightLimit,
  isKeyboardCovering,
  isViewportOversized,
  isViewportSettled,
  layoutHeight,
  readViewportMetrics,
} from './viewport-metrics';

/**
 * iOS animates its keyboard over 0.25s on UIKit's own curve, and
 * `keyboardWillShow` fires BEFORE that animation starts. Driving the shell with
 * the same duration and curve, in the frame that event arrives, makes the shell
 * and the keyboard one movement — instead of the web layer chasing
 * `visualViewport`, which iOS delivers late and in two or three coarse steps.
 */
const KEYBOARD_ANIMATION_MS = 250;
const KEYBOARD_EASING = 'cubic-bezier(0.17, 0.59, 0.4, 0.77)';

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
  document.documentElement.removeAttribute('data-rotating');
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
    // Every clock in the app is stopped for the duration (see `index.css`): an
    // entry keyframe or a height transition that keeps playing through the flip
    // animates FROM the old geometry into the new one, which is the sloshing.
    document.documentElement.setAttribute('data-rotating', '');
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
  // The web signals say a flip HAPPENED; UIKit says what it will end at. The
  // host (see `native-viewport.ts`) hands over the exact post-rotation size in
  // `viewWillTransition`, before a frame is drawn, and reports the coordinator's
  // completion — so the window opens on the first frame and closes on the real
  // reflow instead of on "the numbers held still", which a stale viewport also
  // satisfies. Absent on web/Android, where the media query keeps carrying it.
  installNativeViewport();
  onNativeViewport((box) => {
    if (box.phase === 'rotate') {
      beginRotation();
      return;
    }
    if (rotating && isViewportSettled(readViewportMetrics())) endRotation();
  });
  window.matchMedia?.('(orientation: portrait)').addEventListener('change', beginRotation);
  window.addEventListener('orientationchange', beginRotation);
}

/**
 * Scroll anchoring for a rotation.
 *
 * A rotation rewraps every line in a scroller, so the pixel `scrollTop` the
 * reader was parked at stops pointing at the content they were reading. The
 * browser's own anchoring is not an option in the transcript (it runs with
 * `overflow-anchor: none` so that prepending earlier turns stays stable), so
 * remember one child and its distance above the fold, and put it back.
 *
 * Structurally typed on purpose: only these few numbers matter, which keeps the
 * arithmetic verifiable without a DOM.
 */
type AnchorChild = { offsetTop: number; offsetHeight: number; isConnected: boolean };
type AnchorHost = { children: ArrayLike<Element> };
type AnchorScroller = { scrollTop: number };
export type ScrollAnchor = { el: AnchorChild; offset: number };

/**
 * The top-most child still on screen, plus its (negative or zero) offset from
 * the fold. Stacked children make `offsetTop` monotonic, so this is a binary
 * search rather than a walk — it runs on every scroll event and a long
 * transcript has hundreds of turns.
 */
export function scrollAnchorFor(
  viewport: AnchorScroller,
  container: AnchorHost,
): ScrollAnchor | null {
  const children = container.children;
  const target = viewport.scrollTop;
  let lo = 0;
  let hi = children.length - 1;
  let found: AnchorChild | null = null;
  while (lo <= hi) {
    const mid = (lo + hi) >> 1;
    const child = children[mid] as unknown as AnchorChild;
    if (child.offsetTop + child.offsetHeight > target) {
      found = child;
      hi = mid - 1;
    } else {
      lo = mid + 1;
    }
  }
  return found ? { el: found, offset: found.offsetTop - target } : null;
}

/**
 * Puts the anchored child back where it was. False means there was nothing to
 * restore (no anchor, or the turn was unmounted meanwhile) and the caller owns
 * the fallback.
 */
export function applyScrollAnchor(
  viewport: AnchorScroller,
  anchor: ScrollAnchor | null,
): boolean {
  if (!anchor || !anchor.el.isConnected) return false;
  const next = Math.max(0, anchor.el.offsetTop - anchor.offset);
  if (Math.abs(next - viewport.scrollTop) > 0.5) viewport.scrollTop = next;
  return true;
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
 * Pins the app shell to the *visual* viewport.
 *
 * Focusing the composer shrinks the visual viewport — the software keyboard, or
 * with a hardware keyboard just the form accessory bar — and iOS then slides
 * that visual viewport down over the unchanged layout viewport instead of
 * resizing it. A plain `100dvh` shell keeps its layout-viewport box, so the
 * header rides up under the status bar / Dynamic Island and its
 * `env(safe-area-inset-top)` padding goes off-screen with it.
 *
 * The returned style gives the shell the exact visible height and shifts it
 * back onto the visual viewport, so the header stays put and the composer sits
 * directly on the keyboard. It is `undefined` whenever the visual viewport
 * still matches the layout one, so desktop and idle mobile keep the plain
 * `h-dvh` box (and no transform containing block).
 *
 * Backgrounding the app freezes those metrics: iOS suspends the webview mid
 * keyboard-teardown and, on resume, neither `resize` nor `scroll` fires. The
 * shell would stay pinned to a viewport that no longer exists — header under
 * the status bar, tab bar pushed off the bottom, nothing tappable. So every
 * wake signal re-measures, and a hidden document never records a box at all.
 *
 * It also publishes `--safe-bottom`: the real bottom inset normally, `0px`
 * while the keyboard covers the home indicator, so a footer does not reserve a
 * dead band above the keyboard.
 */
// The height the shell is currently pinned to, or null when it is the plain
// `h-dvh` box. With the native keyboard driver the webview is NOT resized
// (`resize: 'none'`), so `visualViewport.height` no longer reports the
// keyboard: anything that needs to know "did the shell just change size?" has
// to ask here instead of measuring the visual viewport.
let pinnedShellHeight: number | null = null;

/** The shell's current height in CSS pixels, keyboard pin included. */
export function shellViewportHeight(): number {
  const metrics = readViewportMetrics();
  return clampShellHeight(pinnedShellHeight ?? metrics.visualHeight, metrics);
}

export function useVisualViewportShell(): CSSProperties | undefined {
  const [box, setBox] = useState<Box | null>(null);
  // True only while the native keyboard animation is running, so the pin
  // transitions with the keyboard and snaps for everything else (rotation,
  // wake, a toolbar appearing).
  const [animating, setAnimating] = useState(false);

  useEffect(() => {
    const vv = window.visualViewport;
    if (!vv) return;

    let frame = 0;
    // iOS only: Android resizes the webview itself, so `visualViewport` already
    // tells the whole truth there and a second driver would subtract the
    // keyboard twice.
    const nativeKeyboard = Capacitor.getPlatform() === 'ios';
    // Set while the native driver owns the box.
    let keyboardPinned = false;
    // What the keyboard is doing, independent of whether the shell is currently
    // pinned to it: a rotation has to drop the pin (the numbers behind it belong
    // to the other orientation) and then rebuild it from fresh ones.
    let keyboardUp = false;
    let keyboardHeight = 0;
    // Assigned only by the native driver below; a no-op elsewhere.
    let repinKeyboard: () => boolean = () => false;
    let timers: number[] = [];
    const clearTimers = () => {
      for (const t of timers) window.clearTimeout(t);
      timers = [];
    };

    const sync = () => {
      cancelAnimationFrame(frame);
      frame = requestAnimationFrame(() => {
        // Mid-rotation iOS reports the PRE-rotation visual viewport against the
        // POST-rotation layout one. That reads as a keyboard, and pinning the
        // shell to it fixes the app at a height the device no longer has —
        // which is the mismatch you see, corrected a few frames later as a
        // jump. Let the plain `h-dvh` box carry the rotation and re-measure
        // once it is over.
        //
        // This runs BEFORE the keyboard pin, and releases it too. A pin is
        // `innerHeight - keyboardHeight` measured in ONE orientation; after the
        // flip it is a height the device does not have, and iOS does not
        // reliably re-announce the keyboard for a rotation. With the keyboard
        // check first, nothing inside the rotation window could ever reach
        // here, so the shell stayed portrait-tall inside a landscape window:
        // the composer sat off the bottom edge and the transcript ran past it.
        // Only this screen keeps a field focused, which is why the session list
        // rotated cleanly. The keyboard is re-pinned from fresh numbers when
        // the rotation ends.
        if (isViewportRotating()) {
          keyboardPinned = false;
          setAnimating(false);
          resetLayoutScroll();
          document.documentElement.style.setProperty(
            '--safe-bottom',
            'env(safe-area-inset-bottom)',
          );
          // `h-dvh` carries the flip — except while the layout viewport is
          // TALLER than the device the OS says we are on, which is most of a
          // portrait→landscape rotation: WebKit swaps the screen immediately
          // and re-lays the viewport out several frames later, so an unpinned
          // shell spends the whole animation hanging off the bottom of the
          // screen (composer and tab bar below the fold) and then snaps back.
          // The screen edge is not a transitional number — unlike the visual
          // viewport this used to measure — so it cannot pin a box that never
          // existed.
          const rotatingMetrics = readViewportMetrics();
          const limit = isViewportOversized(rotatingMetrics)
            ? deviceHeightLimit(rotatingMetrics)
            : null;
          pinnedShellHeight = limit;
          setBox((prev) => {
            if (limit === null) return null;
            return prev && prev.height === limit && prev.top === 0
              ? prev
              : { height: limit, top: 0 };
          });
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
        const covered = isKeyboardCovering(metrics);
        // A resume can hand back a layout viewport TALLER than the device (see
        // `deviceHeightLimit`), and iOS announces nothing afterwards. The plain
        // `h-dvh` box would then hang off the bottom of the screen with the tab
        // bar and the composer below the fold, so pin the shell to the height
        // the device actually has.
        const oversized = isViewportOversized(metrics);
        // Keyboard down means the layout viewport belongs at its origin again.
        if (!covered) resetLayoutScroll();
        // A `position: fixed` shell resolves against the LAYOUT viewport, and
        // iOS scrolls THAT viewport to reveal a focused field — it does it even
        // with `html { overflow: hidden }`. So the whole shell slides up by that
        // offset the frame you tap the composer and snaps back when you leave
        // it, which is the header jumping in and out from under the status bar.
        // `offsetTop` alone never sees it (it is measured against the very
        // viewport that moved), so the pin has to add the scroll back.
        const top = Math.round(vv.offsetTop + layoutScroll());
        const next: Box | null =
          vv.height > 0 && (covered || oversized || top > 1)
            ? { height: clampShellHeight(vv.height, metrics), top }
            : null;
        pinnedShellHeight = next ? next.height : null;
        setBox((prev) =>
          prev && next && prev.height === next.height && prev.top === next.top ? prev : next,
        );
        document.documentElement.style.setProperty(
          '--safe-bottom',
          covered ? '0px' : 'env(safe-area-inset-bottom)',
        );
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
      if (keyboardPinned && !keyboardUp) {
        keyboardPinned = false;
        setAnimating(false);
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
    // visual viewport does not follow, so the caret can land off-screen. Settle
    // the box first, then bring the field itself back into its own scroller.
    const onFocusIn = (event: FocusEvent) => {
      const el = event.target as HTMLElement | null;
      if (!el) return;
      const tag = el.tagName;
      const isField =
        tag === 'TEXTAREA' ||
        el.isContentEditable ||
        (tag === 'INPUT' &&
          !NON_TEXT_INPUT_TYPES.has((el as HTMLInputElement).type.toLowerCase()));
      if (!isField) return;
      // Native iOS: `keyboardWillShow` pins the shell and WKWebView's own
      // scroll-to-reveal is disabled, so the field is already inside the visible
      // box. Measuring here would only fight the running animation.
      if (nativeKeyboard) return;
      resync();
      timers.push(
        window.setTimeout(() => {
          if (document.activeElement !== el) return;
          el.scrollIntoView({ block: 'nearest' });
          // `scrollIntoView` walks EVERY scrollable ancestor, and the last one
          // is the layout viewport: reveal a field this way and the fixed shell
          // travels with it. Scroll the field inside its own scroller, then put
          // the shell's frame of reference back.
          resetLayoutScroll();
          sync();
        }, 350),
      );
    };

    sync();
    vv.addEventListener('resize', sync);
    vv.addEventListener('scroll', sync);
    document.addEventListener('visibilitychange', onVisible);
    window.addEventListener('pageshow', resync);
    window.addEventListener('focus', onVisible);
    window.addEventListener('resize', sync);
    document.addEventListener('focusin', onFocusIn);
    // A layout-viewport scroll moves the fixed shell but fires no visual
    // viewport event, so it has to be watched on its own.
    window.addEventListener('scroll', sync, true);

    // Rotation, not a wake: `start` drops the stale pin in the frame the flip
    // happens, the settles re-check cheaply, and only the end pays for a full
    // wake schedule.
    const stopRotation = onViewportRotation((phase) => {
      if (phase !== 'end') {
        sync();
        return;
      }
      // The rotation is over and both numbers finally describe the same device.
      // A keyboard that survived the flip owns the box again — re-pin it here,
      // because `sync()` cannot see a native keyboard at all (`resize: 'none'`)
      // and would leave the shell full-height with the composer behind it.
      if (repinKeyboard()) return;
      resync();
    });

    // Native measurements arrive exactly where the web layer gets no event at
    // all: a resume that left the layout viewport stale for good, an iPad split
    // view or Stage Manager resize. A resume pays for the full wake schedule
    // (it may also have to release a keyboard pin that never got its `did`
    // event); anything else is one cheap re-measure against the new box.
    const stopNative = onNativeViewport((box) => {
      if (box.phase === 'resume') resync();
      else sync();
    });

    // Native iOS keyboard: one movement, on the OS's own timings.
    //
    // `visualViewport` is the only keyboard signal the web layer has, and on iOS
    // it arrives late and in coarse steps — the shell lands after the keyboard,
    // in two or three visible hops, going up and coming back down. The Capacitor
    // Keyboard plugin reports `keyboardWillShow` with the final height BEFORE
    // UIKit starts animating, so the shell can cover the same distance over the
    // same curve and the two move together.
    //
    // `resize: 'none'` (capacitor.config.mts) keeps WKWebView at full size so
    // nothing resizes the webview underneath that animation, and `setScroll`
    // turns off the native scroll-to-reveal that used to drag the whole fixed
    // shell up by the layout viewport's scroll offset — the second jump.
    let disposeKeyboard = () => {};
    if (nativeKeyboard) {
      const subs: Promise<PluginListenerHandle>[] = [];
      const pin = (height: number) => {
        clearTimers();
        cancelAnimationFrame(frame);
        setAnimating(true);
        // `innerHeight` is the caller's ruler and a resumed webview can report
        // one taller than the screen, so the keyboard pin is clamped too.
        const next = { height: clampShellHeight(height, readViewportMetrics()), top: 0 };
        pinnedShellHeight = next.height;
        setBox((prev) =>
          prev && prev.height === next.height && prev.top === next.top ? prev : next,
        );
      };
      const onWillShow = (info: KeyboardInfo) => {
        keyboardUp = true;
        keyboardHeight = info.keyboardHeight;
        // Mid-rotation the two operands describe different devices — iOS reports
        // the new keyboard height while `innerHeight` is still the old window,
        // or the reverse. Record the height and let the rotation's `end` do the
        // arithmetic once they agree.
        if (isViewportRotating()) return;
        keyboardPinned = true;
        document.documentElement.style.setProperty('--safe-bottom', '0px');
        pin(layoutHeight(readViewportMetrics()) - keyboardHeight);
      };
      const onWillHide = () => {
        keyboardUp = false;
        document.documentElement.style.setProperty(
          '--safe-bottom',
          'env(safe-area-inset-bottom)',
        );
        pin(layoutHeight(readViewportMetrics()));
      };
      // Rebuild the keyboard pin from post-rotation numbers. Snaps rather than
      // transitions: the rotation already moved everything at once.
      repinKeyboard = () => {
        if (!keyboardUp) return false;
        keyboardPinned = true;
        document.documentElement.style.setProperty('--safe-bottom', '0px');
        pin(layoutHeight(readViewportMetrics()) - keyboardHeight);
        setAnimating(false);
        return true;
      };
      const onDidShow = () => setAnimating(false);
      const onDidHide = () => {
        keyboardPinned = false;
        setAnimating(false);
        // Back to the plain `h-dvh` box: same height the animation just reached,
        // so dropping the pin is invisible.
        pinnedShellHeight = null;
        setBox(null);
        sync();
      };
      try {
        subs.push(
          Keyboard.addListener('keyboardWillShow', onWillShow),
          Keyboard.addListener('keyboardWillHide', onWillHide),
          Keyboard.addListener('keyboardDidShow', onDidShow),
          Keyboard.addListener('keyboardDidHide', onDidHide),
        );
        void Keyboard.setScroll({ isDisabled: true }).catch(() => undefined);
      } catch {
        /* plugin unavailable */
      }
      disposeKeyboard = () => {
        for (const sub of subs) void sub.then((handle) => handle.remove()).catch(() => undefined);
      };
    }

    // Native resume: Capacitor fires this on iOS/Android even when the webview
    // emits no viewport event at all. No-op on the web build.
    let disposeNative = () => {};
    try {
      void App.addListener('resume', resync)
        .then((sub) => {
          disposeNative = () => void sub.remove();
        })
        .catch(() => undefined);
    } catch {
      /* plugin unavailable */
    }

    return () => {
      cancelAnimationFrame(frame);
      clearTimers();
      vv.removeEventListener('resize', sync);
      vv.removeEventListener('scroll', sync);
      document.removeEventListener('visibilitychange', onVisible);
      window.removeEventListener('pageshow', resync);
      window.removeEventListener('focus', onVisible);
      window.removeEventListener('resize', sync);
      document.removeEventListener('focusin', onFocusIn);
      window.removeEventListener('scroll', sync, true);
      stopRotation();
      stopNative();
      disposeNative();
      disposeKeyboard();
    };
  }, []);

  if (!box) return undefined;
  const style: CSSProperties = {
    height: `${box.height}px`,
    transform: `translateY(${box.top}px)`,
  };
  if (!animating) return style;
  const curve = `${KEYBOARD_ANIMATION_MS}ms ${KEYBOARD_EASING}`;
  return { ...style, transition: `height ${curve}, transform ${curve}` };
}
