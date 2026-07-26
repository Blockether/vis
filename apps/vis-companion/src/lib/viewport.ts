import { useEffect, useState } from 'react';
import type { CSSProperties } from 'react';

/** Shrink (px) that counts as the keyboard — not a toolbar rounding wobble. */
const COVERED_EPSILON = 12;

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
    const sync = () => {
      cancelAnimationFrame(frame);
      frame = requestAnimationFrame(() => {
        const covered = window.innerHeight - vv.height > COVERED_EPSILON;
        const next: Box | null =
          covered || vv.offsetTop > 1
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

    sync();
    vv.addEventListener('resize', sync);
    vv.addEventListener('scroll', sync);
    return () => {
      cancelAnimationFrame(frame);
      vv.removeEventListener('resize', sync);
      vv.removeEventListener('scroll', sync);
    };
  }, []);

  return box ? { height: `${box.height}px`, transform: `translateY(${box.top}px)` } : undefined;
}
