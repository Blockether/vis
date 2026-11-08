import { seatScroller } from './viewport';

/**
 * How far a glide moves, in heights of the scroller. A longer move is seated this far
 * from its target first, so only the last screen of it is in motion: a smooth move over
 * 40 000 px of transcript paints every page that it passes and reads as a blur.
 */
const GLIDE_SCREENS = 1;

/**
 * A glide whose scroller sent no `scroll` event for this long has stopped. iOS 18 sends
 * no `scrollend`, and without this rule its glides ended only at a 1 s timer. A glide
 * sends one `scroll` event in each frame.
 */
const GLIDE_REST_MS = 200;

/** Does this reader want the scroller to move, not to jump? */
export function wantsScrollMotion(view: Window | null): boolean {
  return !view?.matchMedia?.('(prefers-reduced-motion: reduce)').matches;
}

/**
 * Move `viewport` to the `scrollTop` that `top` gives, in one short, smooth movement.
 *
 * Reduced motion puts the scroller there at once. `onEnd` runs one time, when the
 * movement stops: at the target, or where a hand or a script write stopped it. The
 * smooth part starts in the next frame, because Chromium sends a `scrollend` for the
 * seat, and a listener that heard it would end the glide at its start. Returns the
 * function that stops the listening; it does not stop the scroller.
 */
export function glideTo(viewport: HTMLElement, top: () => number, onEnd?: () => void): () => void {
  const view = viewport.ownerDocument.defaultView;
  const target = () => Math.max(0, Math.min(viewport.scrollHeight - viewport.clientHeight, top()));
  let isDone = false;
  let frame: number | null = null;
  let restTimer: number | null = null;
  const stop = () => {
    isDone = true;
    if (frame !== null) view?.cancelAnimationFrame(frame);
    if (restTimer !== null) view?.clearTimeout(restTimer);
    viewport.removeEventListener('scrollend', end);
    viewport.removeEventListener('scroll', rest);
  };
  const rest = () => {
    if (restTimer !== null) view?.clearTimeout(restTimer);
    restTimer = view?.setTimeout(end, GLIDE_REST_MS) ?? null;
  };
  const end = () => {
    if (isDone) return;
    stop();
    onEnd?.();
  };
  const distance = target() - viewport.scrollTop;
  if (!view || Math.abs(distance) < 1 || !wantsScrollMotion(view)) {
    seatScroller(viewport, target());
    end();
    return stop;
  }
  const reach = viewport.clientHeight * GLIDE_SCREENS;
  if (Math.abs(distance) > reach) seatScroller(viewport, target() - Math.sign(distance) * reach);
  frame = view.requestAnimationFrame(() => {
    frame = null;
    const to = target();
    if (Math.abs(to - viewport.scrollTop) < 1) {
      end();
      return;
    }
    viewport.addEventListener('scrollend', end);
    viewport.addEventListener('scroll', rest, { passive: true });
    rest();
    viewport.scrollTo({ top: to, behavior: 'smooth' });
  });
  return stop;
}
