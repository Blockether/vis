import { useEffect, useState } from 'react';
import type { CSSProperties } from 'react';
import { App } from '@capacitor/app';

/** Shrink (px) that counts as the keyboard — not a toolbar rounding wobble. */
const COVERED_EPSILON = 12;

/**
 * Re-measure schedule after a wake (backgrounded app, tab switch, bfcache
 * restore). iOS hands the webview stale `visualViewport` metrics for a few
 * frames after resume and fires no further resize/scroll, so one measurement is
 * not enough — settle over roughly half a second.
 */
const WAKE_RESYNC_MS = [0, 60, 160, 320, 600];

type Box = { height: number; top: number };

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
export function useVisualViewportShell(): CSSProperties | undefined {
  const [box, setBox] = useState<Box | null>(null);

  useEffect(() => {
    const vv = window.visualViewport;
    if (!vv) return;

    let frame = 0;
    let timers: number[] = [];
    const clearTimers = () => {
      for (const t of timers) window.clearTimeout(t);
      timers = [];
    };

    const sync = () => {
      cancelAnimationFrame(frame);
      frame = requestAnimationFrame(() => {
        // A suspended/hidden webview reports whatever it froze at; recording it
        // would outlive the state it described.
        if (document.visibilityState === 'hidden') return;
        const covered = window.innerHeight - vv.height > COVERED_EPSILON;
        const next: Box | null =
          vv.height > 0 && (covered || vv.offsetTop > 1)
            ? { height: Math.round(vv.height), top: Math.round(vv.offsetTop) }
            : null;
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
        (tag === 'INPUT' && !/^(button|checkbox|radio|submit|reset|file|range|color)$/i.test((el as HTMLInputElement).type));
      if (!isField) return;
      resync();
      timers.push(
        window.setTimeout(() => {
          if (document.activeElement === el) el.scrollIntoView({ block: 'nearest' });
        }, 350),
      );
    };

    sync();
    vv.addEventListener('resize', sync);
    vv.addEventListener('scroll', sync);
    document.addEventListener('visibilitychange', onVisible);
    window.addEventListener('pageshow', resync);
    window.addEventListener('focus', onVisible);
    window.addEventListener('orientationchange', resync);
    window.addEventListener('resize', sync);
    document.addEventListener('focusin', onFocusIn);

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
      window.removeEventListener('orientationchange', resync);
      window.removeEventListener('resize', sync);
      document.removeEventListener('focusin', onFocusIn);
      disposeNative();
    };
  }, []);

  return box ? { height: `${box.height}px`, transform: `translateY(${box.top}px)` } : undefined;
}
