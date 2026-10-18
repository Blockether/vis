import {
  useCallback,
  useEffect,
  useLayoutEffect,
  useRef,
  type MouseEvent as ReactMouseEvent,
  type PointerEvent as ReactPointerEvent,
  type RefObject,
} from 'react';
import {
  DOUBLE_TAP_PX,
  FIT_SCALE,
  NO_TRANSFORM,
  clampTransform,
  distance,
  isDoubleTap,
  panFrom,
  panTransform,
  pinchFrom,
  pinchTransform,
  swipeFrom,
  swipeShift,
  swipeStep,
  toggledZoom,
  transformCss,
  wheelFactor,
  zoomLabel,
  zoomedAbout,
  zoomedBy,
  type Gesture,
  type Point,
  type Tap,
  type Transform,
} from './zoom-pan';

/**
 * ONE ZOOM FOR EVERY PICTURE THAT CAN BE ZOOMED.
 *
 * The drawing viewer (`ImageViewer`) and a PDF page (`PdfArtifact`) both take the
 * same pinch, wheel, trackpad, double tap and drag from this hook, so a gesture
 * that works on one works the same way on the other. The geometry comes from
 * `lib/zoom-pan`; this hook only decides WHEN to ask for it, and writes the
 * result to the element's style.
 */

/** The class of the frame that takes the gestures: the browser keeps none of them. */
export const ZOOM_FRAME_CLASS = 'select-none overflow-hidden overscroll-none [touch-action:none]';

/** The class of the zoomed element: a snap from a button glides, a finger does not. */
export const ZOOMED_CLASS =
  'origin-center transform-gpu transition-transform duration-100 motion-reduce:transition-none';

/** What a pen needs from the surface it draws on (`AnnotationSurface` is one). */
export interface ZoomPen {
  beginAt(clientX: number, clientY: number): void;
  extendTo(clientX: number, clientY: number): void;
  endStroke(): void;
  cancelStroke(): void;
}

/** The neighbours a fitted picture can be swiped to. */
export interface ZoomSwipe {
  back: boolean;
  forward: boolean;
  /** -1 the previous picture, 1 the next one. Called only toward a neighbour. */
  onStep: (direction: -1 | 1) => void;
}

export interface ZoomPanOptions {
  /** A new value is a new picture: it opens fitted again. */
  resetKey: unknown;
  /** The pen is out: a lone pointer draws, and only a pinch still zooms. */
  pen?: RefObject<ZoomPen | null> | null;
  /** A fitted picture with neighbours walks to them with a sideways drag. */
  swipe?: ZoomSwipe | null;
  /** A pointer that lands where this says yes belongs to somebody else. */
  skips?: (at: Point, frame: DOMRect) => boolean;
  /** Every new zoom and pan, for a picture that draws itself again when it rests. */
  onChange?: (transform: Transform) => void;
}

/** A double click this soon after a double tap is the same gesture, sent twice. */
const TAP_ECHO_MS = 500;

/** How long a wheel burst has to stop before the snap transition comes back. */
const WHEEL_REST_MS = 120;

/**
 * Safari's own trackpad pinch. No other engine sends it, and TypeScript's DOM
 * library does not declare it, but it is the ONLY way a Safari pinch reaches a page:
 * the ctrl+wheel Chrome and Firefox send never arrives there.
 */
type SafariGestureEvent = Event & { scale: number; clientX: number; clientY: number };

export function useZoomPan(options: ZoomPanOptions) {
  const viewportRef = useRef<HTMLDivElement | null>(null);
  const transformedRef = useRef<HTMLDivElement | null>(null);
  const labelRef = useRef<HTMLSpanElement | null>(null);
  const transformRef = useRef<Transform>({ ...NO_TRANSFORM });
  const pointersRef = useRef(new Map<number, Point>());
  const gestureRef = useRef<Gesture>(null);
  const shiftRef = useRef(0);
  // One finger that has not travelled yet, the tap that came before it, and when
  // the last double tap zoomed.
  const tapRef = useRef<Tap | null>(null);
  const lastTapRef = useRef<Tap | null>(null);
  const tappedAtRef = useRef(Number.NEGATIVE_INFINITY);
  const latest = useRef(options);
  useLayoutEffect(() => {
    latest.current = options;
  });

  // The transform is written to style rather than to state: a pinch that
  // re-rendered React on every frame would stutter on exactly the devices that
  // pinch. A live swipe rides the same one property — the zoom and pan the
  // picture is at, plus however far a finger has carried it toward its
  // neighbour — so both are painted together instead of overwriting each other.
  const paint = useCallback(() => {
    const node = transformedRef.current;
    if (!node) return;
    const css = transformCss(transformRef.current);
    node.style.transform = shiftRef.current
      ? `translate3d(${shiftRef.current}px, 0, 0) ${css}`
      : css;
  }, []);

  // A zoomed element that mounts later (a page that finished rendering) is painted too.
  useLayoutEffect(() => paint());

  const apply = useCallback(
    (next: Transform) => {
      const transform = clampTransform(next);
      transformRef.current = transform;
      paint();
      if (labelRef.current) labelRef.current.textContent = zoomLabel(transform);
      latest.current.onChange?.(transform);
    },
    [paint],
  );

  /** How far a live swipe has dragged the picture; 0 puts it back in its frame. */
  const slide = useCallback(
    (shift: number) => {
      shiftRef.current = shift;
      paint();
    },
    [paint],
  );

  const reset = useCallback(() => {
    pointersRef.current.clear();
    gestureRef.current = null;
    tapRef.current = null;
    shiftRef.current = 0;
    apply({ ...NO_TRANSFORM });
  }, [apply]);

  const { resetKey } = options;
  useLayoutEffect(() => reset(), [resetKey, reset]);

  const frameCenter = useCallback((): Point => {
    const box = viewportRef.current?.getBoundingClientRect();
    return box ? { x: box.left + box.width / 2, y: box.top + box.height / 2 } : { x: 0, y: 0 };
  }, []);

  const zoomBy = useCallback(
    (factor: number) => apply(zoomedBy(transformRef.current, factor)),
    [apply],
  );

  /** Fitted to 2x about `point`, and back. The pen holds the picture still. */
  const toggle = useCallback(
    (point: Point) => {
      if (latest.current.pen) return;
      apply(toggledZoom(transformRef.current, point, frameCenter()));
    },
    [apply, frameCenter],
  );

  // A live pinch/pan writes the transform every frame; the CSS transition meant
  // for button/reset snaps fights that at exactly the frame rate a finger moves,
  // which is what read as stutter and a lagging, rubber-banded pinch. Only a
  // finger on the glass suspends it.
  const setGliding = (isGliding: boolean) => {
    if (transformedRef.current) {
      transformedRef.current.style.transitionDuration = isGliding ? '' : '0ms';
    }
  };

  function onPointerDownCapture(event: ReactPointerEvent<HTMLDivElement>) {
    const point = { x: event.clientX, y: event.clientY };
    const { pen, swipe, skips } = latest.current;
    const frame = event.currentTarget.getBoundingClientRect();
    if (pointersRef.current.size === 0 && skips?.(point, frame)) return;
    pointersRef.current.set(event.pointerId, point);
    const [a, b] = [...pointersRef.current.values()];
    const pinching = Boolean(a && b);
    const isTouch = event.pointerType === 'touch' || event.pointerType === 'pen';
    tapRef.current = !pinching && isTouch ? { at: point, time: event.timeStamp } : null;
    // A lone finger while drawing is a stroke, and it is the VIEWPORT that owns
    // the pointer: the sheet only covers the picture, so a stroke begun on the
    // dark margin beside it would otherwise never start and the line would only
    // appear for someone who touched down exactly inside the edge. Only a SECOND
    // finger starts a gesture here, so a pinch still works while the pen is out.
    if (pen && !pinching) {
      event.preventDefault();
      event.currentTarget.setPointerCapture(event.pointerId);
      pen.current?.beginAt(event.clientX, event.clientY);
      return;
    }
    // A second finger arrived: the mark the first one was leaving belongs to a
    // pinch, not to the picture. Drop it before the zoom starts, or every
    // two-finger zoom on a phone scribbles a line across the page.
    if (pen && pinching) pen.current?.cancelStroke();
    event.preventDefault();
    event.currentTarget.setPointerCapture(event.pointerId);
    setGliding(false);
    // A picture that is not zoomed in has nothing to pan, so the finger's whole
    // travel means the NEIGHBOURING picture instead. A second finger is a pinch,
    // so whatever the first one had dragged goes back first.
    if (pinching) slide(0);
    const isSwiping = Boolean(swipe) && transformRef.current.scale <= FIT_SCALE;
    gestureRef.current = pinching
      ? pinchFrom(a, b, transformRef.current)
      : isSwiping
        ? swipeFrom(event.pointerId, point)
        : panFrom(event.pointerId, point, transformRef.current);
  }

  function onPointerMove(event: ReactPointerEvent<HTMLDivElement>) {
    if (!pointersRef.current.has(event.pointerId)) return;
    const point = { x: event.clientX, y: event.clientY };
    pointersRef.current.set(event.pointerId, point);
    const tap = tapRef.current;
    if (tap && distance(tap.at, point) > DOUBLE_TAP_PX) tapRef.current = null;
    const { pen, swipe } = latest.current;
    const gesture = gestureRef.current;
    if (pen && !gesture && pointersRef.current.size === 1) {
      event.preventDefault();
      pen.current?.extendTo(event.clientX, event.clientY);
      return;
    }
    if (!gesture) return;
    if (gesture.kind === 'swipe' && gesture.pointerId === event.pointerId) {
      event.preventDefault();
      slide(
        swipeShift(gesture, point, {
          back: swipe?.back ?? false,
          forward: swipe?.forward ?? false,
        }),
      );
      return;
    }
    const [a, b] = [...pointersRef.current.values()];
    if (gesture.kind === 'pinch' && a && b) {
      event.preventDefault();
      apply(pinchTransform(gesture, a, b));
    } else if (!pen && gesture.kind === 'pan' && gesture.pointerId === event.pointerId) {
      event.preventDefault();
      apply(panTransform(gesture, point));
    }
  }

  function onPointerUp(event: ReactPointerEvent<HTMLDivElement>) {
    if (!pointersRef.current.has(event.pointerId)) return;
    const { pen, swipe } = latest.current;
    const gesture = gestureRef.current;
    pointersRef.current.delete(event.pointerId);
    if (pen) pen.current?.endStroke();
    if (gesture?.kind === 'swipe' && gesture.pointerId === event.pointerId) {
      // Let go with the transition back on, so a swipe that never reached its
      // neighbour GLIDES home instead of snapping there.
      setGliding(true);
      slide(0);
      // A CANCELLED pointer is the system taking the touch away — a call, a
      // notification, the edge of the screen — and never a reader's decision, so
      // the picture goes home and the gallery stays where it was.
      const direction =
        event.type === 'pointercancel'
          ? 0
          : swipeStep(gesture, { x: event.clientX, y: event.clientY });
      const isOpen = direction < 0 ? swipe?.back : swipe?.forward;
      if (direction && isOpen) swipe?.onStep(direction);
    }
    // Lifting one finger of a pinch continues as a pan from where the other one
    // is, instead of jumping the picture on the next move — unless the pen is
    // out, where the remaining finger belongs to the stroke, never a pan.
    const [remaining] = [...pointersRef.current.entries()];
    gestureRef.current =
      remaining && !pen ? panFrom(remaining[0], remaining[1], transformRef.current) : null;
    // Last finger up: restore the CSS transition so a clamp snap-back (e.g.
    // pinching past 1x) and the buttons' own zoom animate again.
    if (!gestureRef.current) setGliding(true);
    // A finger that lands and lifts twice in one place is a double tap: the
    // phone's own way to zoom a picture in and out again.
    const tap = tapRef.current;
    tapRef.current = null;
    if (!tap || remaining || event.type === 'pointercancel') return;
    const done = { at: tap.at, time: event.timeStamp };
    const isDouble = isDoubleTap(lastTapRef.current, done);
    lastTapRef.current = isDouble ? null : done;
    if (!isDouble) return;
    tappedAtRef.current = event.timeStamp;
    toggle(done.at);
  }

  // On a desk the double click is the double tap. A phone may send it too, after
  // the double tap already zoomed.
  function onDoubleClick(event: ReactMouseEvent<HTMLDivElement>) {
    if (event.timeStamp - tappedAtRef.current < TAP_ECHO_MS) return;
    toggle({ x: event.clientX, y: event.clientY });
  }

  /**
   * THE WHEEL AND THE TRACKPAD ARE TAKEN NATIVELY, NON-PASSIVE.
   *
   * React registers `onWheel` PASSIVE at the root, so a `preventDefault()` inside it
   * is dropped on the floor and the browser zooms the whole page under the open
   * picture — which on Safari, where a page zoom does not reset, is the "not
   * reliable" half of the report. Safari also reports a trackpad pinch as its own
   * `gesture*` events INSTEAD of the ctrl+wheel every other engine sends, so without
   * these three listeners the viewer never sees a Safari pinch at all.
   *
   * Both paths go through {@link zoomedAbout}, so the pixel under the pointer stays
   * under it, and through {@link wheelFactor}, so the step follows the distance
   * scrolled instead of the number of events the browser happened to send.
   */
  useEffect(() => {
    const viewport = viewportRef.current;
    if (!viewport) return;

    // A wheel burst writes the transform every frame, and the snap transition meant
    // for the buttons fights it exactly as a finger's does. It is restored once the
    // burst stops — never while a finger still owns the picture.
    let settle: ReturnType<typeof setTimeout> | undefined;
    const direct = () => {
      const node = transformedRef.current;
      if (!node) return;
      node.style.transitionDuration = '0ms';
      clearTimeout(settle);
      settle = setTimeout(() => {
        if (!gestureRef.current) node.style.transitionDuration = '';
      }, WHEEL_REST_MS);
    };

    const onWheel = (event: WheelEvent) => {
      event.preventDefault();
      if (latest.current.pen) return;
      direct();
      apply(
        zoomedAbout(
          transformRef.current,
          wheelFactor(event.deltaY, event.deltaMode, event.ctrlKey),
          { x: event.clientX, y: event.clientY },
          frameCenter(),
        ),
      );
    };

    // A Safari pinch reports its scale CUMULATIVELY from the gesture's start, so the
    // transform it started from is what every change is measured against.
    let base: Transform | null = null;
    let baseScale = 1;
    const onGestureStart = (event: SafariGestureEvent) => {
      event.preventDefault();
      if (latest.current.pen) return;
      base = { ...transformRef.current };
      baseScale = event.scale || 1;
      direct();
    };
    const onGestureChange = (event: SafariGestureEvent) => {
      event.preventDefault();
      if (!base) return;
      direct();
      apply(
        zoomedAbout(
          base,
          (event.scale || 1) / baseScale,
          { x: event.clientX, y: event.clientY },
          frameCenter(),
        ),
      );
    };
    const onGestureEnd = (event: SafariGestureEvent) => {
      event.preventDefault();
      base = null;
    };

    const gestures: [string, (event: SafariGestureEvent) => void][] = [
      ['gesturestart', onGestureStart],
      ['gesturechange', onGestureChange],
      ['gestureend', onGestureEnd],
    ];
    viewport.addEventListener('wheel', onWheel, { passive: false });
    for (const [type, handler] of gestures) {
      viewport.addEventListener(type, handler as EventListener, { passive: false });
    }
    return () => {
      clearTimeout(settle);
      viewport.removeEventListener('wheel', onWheel);
      for (const [type, handler] of gestures) {
        viewport.removeEventListener(type, handler as EventListener);
      }
    };
  }, [apply, frameCenter]);

  return {
    /** The frame that takes the gestures. */
    viewportRef,
    /** The element that zooms and pans. */
    transformedRef,
    /** Where the zoom is reported, as a percentage. */
    labelRef,
    /** The zoom and pan the picture is at now. */
    transformRef,
    apply,
    reset,
    zoomBy,
    /** Spread these on the frame. */
    handlers: {
      onPointerDownCapture,
      onPointerMove,
      onPointerUp,
      onPointerCancel: onPointerUp,
      onDoubleClick,
    },
  };
}
