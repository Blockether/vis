import {
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
  type PointerEvent as ReactPointerEvent,
  type ReactNode,
} from "react";
import { createPortal } from "react-dom";
import { PEN_COLORS, flattenAnnotations, type PenToken } from "../lib/annotate";
import { copyImage, shareImage, shareVerb } from "../lib/image-share";
import {
  NO_TRANSFORM,
  FIT_SCALE,
  clampTransform,
  panFrom,
  panTransform,
  pinchFrom,
  pinchTransform,
  swipeFrom,
  swipeShift,
  swipeStep,
  transformCss,
  zoomLabel,
  wheelFactor,
  zoomedAbout,
  zoomedBy,
  partPixels,
  visiblePart,
  type Gesture,
  type Point,
  type Transform,
} from "../lib/zoom-pan";
import {
  AnnotationLayer,
  PenToolbar,
  type AnnotationSurface,
} from "./AnnotationLayer";
import { BandButton, Button, DialogHeader } from "./ui";
import { useGalleryStep, type GalleryPicture } from "../lib/gallery";
import { useStickyOverlay } from "../lib/sticky-overlay";

interface ExpandableImageProps {
  src: string;
  alt: string;
  className: string;
  children?: ReactNode;
  /** Extra classes for the zoom trigger itself, e.g. `shrink-0` inside a flex row. */
  frameClassName?: string;
  loading?: "eager" | "lazy";
  decoding?: "async" | "auto" | "sync";
  onError?: () => void;
  /**
   * Given, the viewer can hand the picture BACK: the flattened image (original
   * pixels plus every annotation) for the caller to put where the original was.
   * That is what makes a not-yet-sent attachment editable rather than read-only.
   */
  onApply?: (edited: Blob) => void | Promise<void>;
  /**
   * Where this picture sits in the gallery its call site laid out. Given, and
   * inside an {@link ImageGallery}, the viewer it opens can walk to its
   * neighbours; without one the picture is alone on screen.
   */
  galleryAt?: number;
}

interface ImageViewerProps {
  src: string;
  name: string;
  onClose: () => void;
  onApply?: (edited: Blob) => void | Promise<void>;
  /**
   * What the band's verb says when the picture can go back. "Save" replaces the
   * attachment it came from; a capture of a document has no slot yet, so it says
   * "Attach to message" instead — the promise made to the human has to match what
   * actually happens.
   */
  applyLabel?: string;
  /**
   * The gallery this picture belongs to, in reading order, and `at` is where
   * `src` sits in it. Given two or more, the viewer steps through them with the
   * arrow keys and its own two buttons, so a contact sheet is read in the
   * viewer instead of one close-and-tap per picture.
   */
  pictures?: GalleryPicture[];
  at?: number;
}

export function ExpandableImage({
  src,
  alt,
  className,
  children,
  frameClassName = "",
  loading = "lazy",
  decoding = "async",
  onError,
  onApply,
  galleryAt,
}: ExpandableImageProps) {
  // The picture is identified by its BYTES, not by this instance: when a turn
  // settles, the transcript re-mounts the row that owns this trigger, and a
  // reader looking at the picture must not be dropped back onto the answer.
  //
  // Memoized because `src` may BE the bytes: a composer attachment is a base64
  // data URL, so composing the key on every render allocated — and compared —
  // megabytes for every keystroke of the message being typed beside it.
  const overlayKey = useMemo(() => `image:${src}`, [src]);
  const [open, setOpen] = useStickyOverlay(overlayKey);
  // A picture laid out in a grid is one step of that grid's gallery. An
  // EDITABLE one is not: `onApply` hands the flattened result back to the slot
  // THIS trigger owns, so a viewer that had walked to a neighbour would send
  // the wrong picture back to it.
  const step = useGalleryStep(onApply ? undefined : galleryAt, {
    src,
    name: alt,
  });

  return (
    <>
      <button
        type="button"
        className={`${
          children ? "flex min-w-0 items-center gap-1.5" : "block"
        } max-w-full cursor-zoom-in appearance-none border-0 bg-transparent p-0 text-left ${frameClassName}`}
        onClick={() => setOpen(true)}
        aria-label={`Open ${alt} full screen`}
      >
        <img
          src={src}
          alt={alt}
          loading={loading}
          decoding={decoding}
          onError={onError}
          className={className}
        />
        {children}
      </button>
      {open && (
        <ImageViewer
          src={src}
          name={alt}
          pictures={step?.pictures}
          at={step?.at}
          onApply={onApply}
          onClose={() => setOpen(false)}
        />
      )}
    </>
  );
}

/**
 * Safari's own trackpad pinch. No other engine sends it, and TypeScript's DOM
 * library does not declare it, but it is the ONLY way a Safari pinch reaches a page:
 * the ctrl+wheel Chrome and Firefox send never arrives there.
 */
type SafariGestureEvent = Event & {
  scale: number;
  clientX: number;
  clientY: number;
};

/**
 * A picture, full screen: pinch/scroll/double-click zoom, pan, and a pen. Given
 * `onApply` it also hands the flattened result back, which is what makes an
 * attachment editable and a rendered document page sendable.
 *
 * The viewer itself owns only the SCREEN: which gesture is in flight, what the
 * status line says, which button is busy. The geometry it applies comes from
 * `lib/zoom-pan`, the strokes belong to {@link AnnotationLayer}, flattening to
 * `lib/annotate` and the platform sheets to `lib/image-share` — so each of
 * those can be used, and tested, without opening a full-screen dialog.
 */
export function ImageViewer({
  src,
  name,
  onClose,
  onApply,
  applyLabel = "Save",
  pictures,
  at,
}: ImageViewerProps) {
  const imageRef = useRef<HTMLImageElement | null>(null);
  const transformedRef = useRef<HTMLDivElement | null>(null);
  const zoomLabelRef = useRef<HTMLSpanElement | null>(null);
  const viewportRef = useRef<HTMLDivElement | null>(null);
  const annotationRef = useRef<AnnotationSurface | null>(null);
  const transformRef = useRef<Transform>({ ...NO_TRANSFORM });
  const pointersRef = useRef(new Map<number, Point>());
  const gestureRef = useRef<Gesture>(null);
  const shiftRef = useRef(0);
  const [drawing, setDrawing] = useState(false);
  const [penColor, setPenColor] = useState<PenToken>(PEN_COLORS[0].token);
  const [strokeCount, setStrokeCount] = useState(0);
  const [busy, setBusy] = useState<
    "copy" | "share" | "apply" | "trim" | null
  >(null);
  // Probed once per open: the sheet cannot appear or disappear mid-viewer.
  const [shareAction] = useState(shareVerb);
  const [status, setStatus] = useState("");
  // A gallery of one is not a gallery: nothing to step to, so no stepper, no
  // arrow keys and no position to report.
  const gallery = pictures && pictures.length > 1 ? pictures : null;
  // Where the reader stands is remembered by the picture's BYTES, never by an
  // index: the gallery is discovered as its tiles land, so a neighbour that
  // arrives late renumbers the list under an open viewer. Holding the src keeps
  // the reader on the picture being read; holding a number would slide them off
  // it. Until this picture is registered the props are the whole gallery of one
  // the viewer can show.
  const [shownSrc, setShownSrc] = useState(src);
  // A trim REPLACES the picture on screen with the part of it that was in
  // view: everything after it — the pen, copy, share, apply — belongs to
  // those pixels. The original is never touched, so one tap gets it back.
  const [trimmed, setTrimmed] = useState<{ src: string; name: string } | null>(
    null,
  );
  const trimUrlsRef = useRef<string[]>([]);
  const dropTrim = useCallback(() => {
    for (const url of trimUrlsRef.current) URL.revokeObjectURL(url);
    trimUrlsRef.current = [];
    setTrimmed(null);
  }, []);
  // A trim lives in memory for as long as the viewer is open and not one
  // moment longer — a closed dialog that keeps its blobs is a leak per tap.
  useEffect(() => dropTrim, [dropTrim]);
  const found = gallery
    ? gallery.findIndex((picture) => picture.src === shownSrc)
    : -1;
  const step = found < 0 ? (at ?? 0) : found;
  const untrimmed = (found < 0 ? undefined : gallery?.[found]) ?? { src, name };
  const shown = trimmed ?? untrimmed;
  const hasEdits = strokeCount > 0 || trimmed !== null;

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

  const applyTransform = useCallback(
    (next: Transform) => {
      const transform = clampTransform(next);
      transformRef.current = transform;
      paint();
      if (zoomLabelRef.current)
        zoomLabelRef.current.textContent = zoomLabel(transform);
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

  const resetTransform = useCallback(() => {
    pointersRef.current.clear();
    gestureRef.current = null;
    shiftRef.current = 0;
    applyTransform({ ...NO_TRANSFORM });
  }, [applyTransform]);

  /**
   * Walk to another picture of the same gallery.
   *
   * A step lands on a NEW picture, so it starts the way an opened one does:
   * fitted, unzoomed, and without the previous picture's strokes floating over
   * bytes they were never drawn on. Drawing therefore holds the viewer still —
   * an arrow key mid-sketch would throw the sketch away.
   */
  const stepTo = useCallback(
    (next: number) => {
      if (!gallery || drawing) return;
      const target = Math.min(Math.max(next, 0), gallery.length - 1);
      if (target === step) return;
      annotationRef.current?.clear();
      dropTrim();
      resetTransform();
      setStatus("");
      setShownSrc(gallery[target].src);
    },
    [gallery, drawing, step, resetTransform, dropTrim],
  );

  useEffect(() => {
    const previousOverflow = document.body.style.overflow;
    document.body.style.overflow = "hidden";
    // Left and right are what a reader already presses in front of a gallery,
    // on a phone keyboard as much as on a desktop one, so the viewer answers
    // them instead of making the reader close it once per picture.
    const keyDown = (event: KeyboardEvent) => {
      if (event.key === "Escape") onClose();
      if (event.key === "ArrowLeft") stepTo(step - 1);
      if (event.key === "ArrowRight") stepTo(step + 1);
    };
    window.addEventListener("keydown", keyDown);
    return () => {
      document.body.style.overflow = previousOverflow;
      window.removeEventListener("keydown", keyDown);
    };
  }, [onClose, stepTo, step]);

  function fitAnnotations() {
    const image = imageRef.current;
    if (image)
      annotationRef.current?.fit(image.naturalWidth, image.naturalHeight);
  }

  function beginGesture(event: ReactPointerEvent<HTMLDivElement>) {
    const point = { x: event.clientX, y: event.clientY };
    pointersRef.current.set(event.pointerId, point);
    const pointers = [...pointersRef.current.values()];
    const [a, b] = pointers;
    const pinching = Boolean(a && b);
    // A lone finger while drawing is a stroke owned by the annotation layer
    // above this handler, not a pan — only a SECOND finger starts a gesture
    // here, so a pinch still works while the pen is out.
    // A lone finger while drawing is a stroke, and it is the VIEWPORT that owns
    // the pointer: the sheet only covers the picture, so a stroke begun on the
    // dark margin beside it would otherwise never start and the line would only
    // appear for someone who touched down exactly inside the edge.
    if (drawing && !pinching) {
      event.preventDefault();
      event.currentTarget.setPointerCapture(event.pointerId);
      annotationRef.current?.beginAt(event.clientX, event.clientY);
      return;
    }
    // A second finger arrived: the mark the first one was leaving belongs to a
    // pinch, not to the picture. Drop it before the zoom starts, or every
    // two-finger zoom on a phone scribbles a line across the page.
    if (drawing && pinching) annotationRef.current?.cancelStroke();
    event.preventDefault();
    event.currentTarget.setPointerCapture(event.pointerId);
    // A live pinch/pan writes the transform every frame; the CSS transition
    // meant for button/reset snaps fights that at exactly the frame rate a
    // finger moves, which is what read as stutter and a lagging, rubber-banded
    // pinch. Only a finger on the glass suspends it.
    if (transformedRef.current)
      transformedRef.current.style.transitionDuration = "0ms";
    // A picture that is not zoomed in has nothing to pan, so the finger's whole
    // travel means the NEIGHBOURING picture instead — the gallery is walked by
    // swiping it, which is why no pair of arrows is left on the toolbar. A second
    // finger is a pinch, so whatever the first one had dragged goes back first.
    if (pinching) slide(0);
    const isSwiping = gallery !== null && transformRef.current.scale <= FIT_SCALE;
    gestureRef.current = pinching
      ? pinchFrom(a, b, transformRef.current)
      : isSwiping
        ? swipeFrom(event.pointerId, point)
        : panFrom(event.pointerId, point, transformRef.current);
  }

  function moveGesture(event: ReactPointerEvent<HTMLDivElement>) {
    if (!pointersRef.current.has(event.pointerId)) return;
    pointersRef.current.set(event.pointerId, {
      x: event.clientX,
      y: event.clientY,
    });
    const gesture = gestureRef.current;
    if (drawing && !gesture && pointersRef.current.size === 1) {
      event.preventDefault();
      annotationRef.current?.extendTo(event.clientX, event.clientY);
      return;
    }
    if (!gesture) return;
    if (gesture.kind === "swipe" && gesture.pointerId === event.pointerId) {
      event.preventDefault();
      slide(
        swipeShift(
          gesture,
          { x: event.clientX, y: event.clientY },
          { back: step > 0, forward: step < (gallery?.length ?? 1) - 1 },
        ),
      );
      return;
    }
    const [a, b] = [...pointersRef.current.values()];
    if (gesture.kind === "pinch" && a && b) {
      event.preventDefault();
      applyTransform(pinchTransform(gesture, a, b));
    } else if (
      !drawing &&
      gesture.kind === "pan" &&
      gesture.pointerId === event.pointerId
    ) {
      event.preventDefault();
      applyTransform(
        panTransform(gesture, { x: event.clientX, y: event.clientY }),
      );
    }
  }

  function endGesture(event: ReactPointerEvent<HTMLDivElement>) {
    const swipe = gestureRef.current;
    pointersRef.current.delete(event.pointerId);
    if (drawing) annotationRef.current?.endStroke();
    if (swipe?.kind === "swipe" && swipe.pointerId === event.pointerId) {
      // Let go with the transition back on, so a swipe that never reached its
      // neighbour GLIDES home instead of snapping there.
      if (transformedRef.current)
        transformedRef.current.style.transitionDuration = "";
      slide(0);
      // A CANCELLED pointer is the system taking the touch away — a call, a
      // notification, the edge of the screen — and never a reader's decision, so
      // the picture goes home and the gallery stays where it was.
      const direction =
        event.type === "pointercancel"
          ? 0
          : swipeStep(swipe, { x: event.clientX, y: event.clientY });
      if (direction) stepTo(step + direction);
    }
    // Lifting one finger of a pinch continues as a pan from where the other one
    // is, instead of jumping the picture on the next move — unless the pen is
    // out, where the remaining finger belongs to the stroke, never a pan.
    const [remaining] = [...pointersRef.current.entries()];
    gestureRef.current =
      remaining && !drawing
        ? panFrom(remaining[0], remaining[1], transformRef.current)
        : null;
    // Last finger up: restore the CSS transition so a clamp snap-back (e.g.
    // pinching past 1x) and the toolbar's own zoom buttons animate again.
    if (!gestureRef.current && transformedRef.current)
      transformedRef.current.style.transitionDuration = "";
  }

  function zoomBy(factor: number) {
    applyTransform(zoomedBy(transformRef.current, factor));
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
    // for the toolbar's buttons fights it exactly as a finger's does. It is restored
    // once the burst stops — never while a finger still owns the picture.
    let settle: ReturnType<typeof setTimeout> | undefined;
    const direct = () => {
      const node = transformedRef.current;
      if (!node) return;
      node.style.transitionDuration = "0ms";
      clearTimeout(settle);
      settle = setTimeout(() => {
        if (!gestureRef.current) node.style.transitionDuration = "";
      }, 120);
    };
    const frameCenter = (): Point => {
      const box = viewport.getBoundingClientRect();
      return { x: box.left + box.width / 2, y: box.top + box.height / 2 };
    };

    const onWheel = (event: WheelEvent) => {
      event.preventDefault();
      if (drawing) return;
      direct();
      applyTransform(
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
      if (drawing) return;
      base = { ...transformRef.current };
      baseScale = event.scale || 1;
      direct();
    };
    const onGestureChange = (event: SafariGestureEvent) => {
      event.preventDefault();
      if (!base) return;
      direct();
      applyTransform(
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
      ["gesturestart", onGestureStart],
      ["gesturechange", onGestureChange],
      ["gestureend", onGestureEnd],
    ];
    viewport.addEventListener("wheel", onWheel, { passive: false });
    for (const [type, handler] of gestures) {
      viewport.addEventListener(type, handler as EventListener, {
        passive: false,
      });
    }
    return () => {
      clearTimeout(settle);
      viewport.removeEventListener("wheel", onWheel);
      for (const [type, handler] of gestures) {
        viewport.removeEventListener(type, handler as EventListener);
      }
    };
  }, [applyTransform, drawing]);

  function toggleZoom() {
    if (drawing) return;
    if (transformRef.current.scale > 1) resetTransform();
    else applyTransform({ scale: 2, x: 0, y: 0 });
  }

  /**
   * Trim the picture to what the frame is SHOWING.
   *
   * Zoom and pan already choose a region; this makes that region the picture,
   * so a detail can be drawn on, copied, shared or sent without the page
   * around it. The crop is taken at the ORIGINAL resolution — the screen only
   * says WHICH pixels, never how many — and any strokes already on the layer
   * are flattened into it, so a trim never throws a mark away.
   */
  async function trimToView() {
    const image = imageRef.current;
    const frame = viewportRef.current;
    if (!image || !frame) return;
    const part = visiblePart(
      image.getBoundingClientRect(),
      frame.getBoundingClientRect(),
    );
    if (!part) {
      setStatus("Zoom in first — the whole picture is already in view.");
      return;
    }
    setBusy("trim");
    setStatus("Trimming…");
    try {
      const blob = await flattenAnnotations(
        image,
        annotationRef.current?.canvas() ?? null,
        part,
      );
      const url = URL.createObjectURL(blob);
      trimUrlsRef.current.push(url);
      const size = partPixels(part, image.naturalWidth, image.naturalHeight);
      // The strokes went INTO the trim; left on the layer they would be
      // painted a second time, stretched across the pixels that survived.
      annotationRef.current?.clear();
      resetTransform();
      setTrimmed({ src: url, name: shown.name });
      setStatus(`Trimmed to ${size.width} × ${size.height}.`);
    } catch (cause) {
      setStatus(
        cause instanceof Error ? cause.message : "Could not trim this image",
      );
    } finally {
      setBusy(null);
    }
  }

  function editedImage(): Promise<Blob> {
    const image = imageRef.current;
    if (!image) throw new Error("Image is not ready");
    return flattenAnnotations(image, annotationRef.current?.canvas() ?? null);
  }

  /**
   * Every finished action is the same three steps — say what is happening, do
   * it, say how it went — so they are said once, and each caller only supplies
   * the work and the sentence to fall back to.
   */
  async function run(
    kind: "copy" | "share" | "apply",
    fallback: string,
    work: (blob: Blob) => Promise<string>,
  ) {
    setBusy(kind);
    setStatus("Preparing image…");
    try {
      setStatus(await work(await editedImage()));
    } catch (cause) {
      setStatus(cause instanceof Error ? cause.message : fallback);
    } finally {
      setBusy(null);
    }
  }

  /**
   * Hand the drawn-on picture back to whoever opened the viewer, and leave. The
   * caller owns the slot it came from, so an annotated attachment REPLACES itself
   * instead of arriving as a second copy, and the original bytes are never mutated
   * here.
   *
   * The pen is put down first, because saving IS the end of drawing — which is why
   * the toggle below offers no second way to finish.
   */
  async function applyEdit() {
    if (!onApply) return;
    setDrawing(false);
    // Save on an untouched pending attachment means "keep this one and leave".
    // Re-encoding a phone photo to a full-resolution PNG did no work for the
    // human and could pin WebKit's main thread long enough to lock the app.
    if (!hasEdits) {
      onClose();
      return;
    }
    await run("apply", "Could not save this edit", async (blob) => {
      await onApply(blob);
      onClose();
      return "";
    });
  }

  const viewer = (
    <div
      className="fixed inset-0 z-[100] isolate bg-ink text-white"
      role="dialog"
      aria-modal="true"
      aria-label={`${shown.name} image viewer`}
    >
      {/* The picture's own title bar is the app's one dialog band — it only has to
          float over the image and clear the notch.

          SAVING IS A CELL OF THAT BAND, ONE HAIRLINE FROM THE ✕. It is the verb that
          ENDS this screen, the way a note's Save does, so it stands where leaving
          stands — not down in the strip of drawing tools, where it read as a SECOND
          finish button beside the pen's own toggle and neither of the two said which
          one kept the ink. */}
      <DialogHeader
        title={shown.name}
        closeLabel={`Close ${shown.name}`}
        onClose={onClose}
        isUnderNotch
        className="absolute inset-x-0 top-0 z-20"
        actions={
          onApply ? (
            <BandButton
              onClick={() => void applyEdit()}
              disabled={busy !== null}
              // Ink on the layer, or a trim taken: the cell wears the accent only
              // when the picture on screen is no longer the picture on the gateway.
              isPrimary={hasEdits}
            >
              {busy === "apply" ? "Saving…" : applyLabel}
            </BandButton>
          ) : null
        }
      />

      {drawing && (
        <div className="absolute inset-x-0 top-[calc(3rem+env(safe-area-inset-top))] z-20 border-b border-dialog-edge bg-panel px-3 sm:top-12 sm:px-4">
          <PenToolbar
            color={penColor}
            onColor={setPenColor}
            strokeCount={strokeCount}
            onUndo={() => annotationRef.current?.undo()}
            onClear={() => annotationRef.current?.clear()}
            className="mx-auto flex max-w-[1400px] items-center justify-center gap-1 overflow-x-auto overscroll-x-contain"
          />
        </div>
      )}

      {/* THE PICTURE CLEARS THE BAND THAT CLEARS THE NOTCH.

          The title bar floats over the image, so the top pad and the picture's
          own cap are what keep it off the glass — and under a notch that band
          is the safe-area inset TALLER (48px + inset). Both therefore carry the
          same inset the band does, and drop it again at `sm:` exactly where the
          band's `sm:pt-0` does, so picture and title never disagree about where
          the top of the screen is.

          HANDWRITING IS NOT A SELECTION.

          Reported from an iPad: circles drew fine, but writing letters selected
          the picture instead of marking it. Letters are many short, quick
          strokes in one small patch — what WebKit's touch recogniser reads as
          the tap-drag that SELECTS, which neither `touch-action` nor a cancelled
          `pointerdown` speaks to. A stroke may start on the dark margin, so the
          viewport declares itself unselectable too, not only the sheet over the
          picture. */}
      <div
        className={`absolute inset-0 grid cursor-grab select-none place-items-center overflow-hidden overscroll-none px-4 active:cursor-grabbing [touch-action:none] ${drawing ? "pb-24 pt-[calc(8rem+env(safe-area-inset-top))] sm:pt-32" : "pb-24 pt-[calc(5rem+env(safe-area-inset-top))] sm:pt-20"}`}
        onPointerDownCapture={beginGesture}
        onPointerMove={moveGesture}
        onPointerUp={endGesture}
        onPointerCancel={endGesture}
        onDoubleClick={toggleZoom}
        ref={viewportRef}
      >
        <div
          ref={transformedRef}
          className="relative inline-block origin-center transform-gpu transition-transform duration-100 motion-reduce:transition-none"
        >
          <img
            ref={imageRef}
            src={shown.src}
            alt={shown.name}
            draggable={false}
            onLoad={fitAnnotations}
            className="block max-h-full max-w-[calc(100vw-2rem)] select-none object-contain"
          />
          <AnnotationLayer
            ref={annotationRef}
            active={drawing}
            color={penColor}
            onStrokesChange={setStrokeCount}
            label="Image annotation layer"
            className="absolute inset-0 size-full"
          />
        </div>
      </div>

      <div className="absolute inset-x-0 bottom-0 z-20 border-t border-dialog-edge bg-panel pb-[max(0.75rem,env(safe-area-inset-bottom))] pl-[max(0.75rem,env(safe-area-inset-left))] pr-[max(0.75rem,env(safe-area-inset-right))] pt-2">
        <div className="mx-auto flex max-w-[1400px] items-center gap-2 overflow-x-auto overscroll-x-contain pb-1">
          {/* One frame, three parts: the segmented group owns the corner and clips
              it, so its segments stay square and no notch opens where two of them
              meet. */}
          <div
            className="flex shrink-0 items-center overflow-hidden rounded-control [&>button]:rounded-none"
            aria-label="Zoom controls"
          >
            <Button
              variant="secondary"
              onClick={() => zoomBy(1 / 1.35)}
              aria-label="Zoom out"
            >
              −
            </Button>
            <Button
              variant="secondary"
              isJoined
              className="min-w-14"
              onClick={resetTransform}
              aria-label="Reset zoom"
            >
              <span ref={zoomLabelRef}>100%</span>
            </Button>
            <Button
              variant="secondary"
              onClick={() => zoomBy(1.35)}
              aria-label="Zoom in"
            >
              +
            </Button>
          </div>

          <Button
            variant={drawing ? "primary" : "secondary"}
            onClick={() => {
              resetTransform();
              setDrawing((current) => !current);
              setStatus("");
            }}
            aria-pressed={drawing}
          >
            {/* One toggle, one state: pressed IS drawing, so it never renames itself
                into a finish verb that competes with the band's Save. */}
            Draw
          </Button>

          <Button
            variant="secondary"
            onClick={trimToView}
            disabled={drawing || busy !== null}
            aria-label="Trim to view"
          >
            {busy === "trim" ? "Trimming…" : "Trim"}
          </Button>

          {trimmed && (
            <Button
              variant="secondary"
              onClick={() => {
                annotationRef.current?.clear();
                dropTrim();
                resetTransform();
                setStatus("");
              }}
              disabled={busy !== null}
              aria-label="Undo trim"
            >
              Undo trim
            </Button>
          )}

          <div className="ml-auto flex shrink-0 items-center gap-2">
            <Button
              variant="secondary"
              onClick={() =>
                run("copy", "Could not copy image", (blob) =>
                  copyImage(blob, shown.name),
                )
              }
              disabled={busy !== null}
            >
              {busy === "copy" ? "Copying…" : "Copy"}
            </Button>
            {/* The strip's one primary is share. The picture's way BACK is the band's
                own cell up top, which wears the band's ink instead of claiming a rank
                down here, so exactly one primary button stands on the screen. */}
            <Button
              variant="primary"
              onClick={() =>
                run("share", "Could not share image", (blob) =>
                  shareImage(blob, shown.name),
                )
              }
              disabled={busy !== null}
            >
              {busy === "share" ? `${shareAction}…` : shareAction}
            </Button>
          </div>
        </div>


        <div
          className="mx-auto min-h-4 max-w-[1400px] truncate pt-1 text-center font-mono text-chip text-dialog-hint"
          aria-live="polite"
        >
          {status ||
            (drawing
              ? onApply
                ? `Draw on the image, then ${applyLabel} at the top.`
                : "Draw on the image, then copy or share it."
              : gallery
                ? `${step + 1} of ${gallery.length} · swipe for the next image, or press ← and →.`
                : "Pinch, scroll, or double-click to zoom, then Trim to keep just that.")}
        </div>
      </div>
    </div>
  );

  return createPortal(viewer, document.body);
}
