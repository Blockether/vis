import {
  useCallback,
  useEffect,
  useRef,
  useState,
  type PointerEvent as ReactPointerEvent,
  type WheelEvent as ReactWheelEvent,
} from 'react';
import { createPortal } from 'react-dom';
import { PEN_COLORS, flattenAnnotations, type PenToken } from '../lib/annotate';
import { copyImage, shareImage, shareVerb } from '../lib/image-share';
import {
  NO_TRANSFORM,
  clampTransform,
  panFrom,
  panTransform,
  pinchFrom,
  pinchTransform,
  transformCss,
  zoomLabel,
  zoomedBy,
  type Gesture,
  type Point,
  type Transform,
} from '../lib/zoom-pan';
import {
  AnnotationLayer,
  PenToolbar,
  type AnnotationSurface,
} from './AnnotationLayer';
import { Button } from './ui';

interface ExpandableImageProps {
  src: string;
  alt: string;
  className: string;
  /** Extra classes for the zoom trigger itself, e.g. `shrink-0` inside a flex row. */
  frameClassName?: string;
  loading?: 'eager' | 'lazy';
  decoding?: 'async' | 'auto' | 'sync';
  onError?: () => void;
  /**
   * Given, the viewer can hand the picture BACK: the flattened image (original
   * pixels plus every annotation) for the caller to put where the original was.
   * That is what makes a not-yet-sent attachment editable rather than read-only.
   */
  onApply?: (edited: Blob) => void | Promise<void>;
}

interface ImageViewerProps {
  src: string;
  name: string;
  onClose: () => void;
  onApply?: (edited: Blob) => void | Promise<void>;
  /**
   * What the primary button says when the picture can go back. "Use edit"
   * replaces the attachment it came from; a capture of a document has no slot
   * yet, so it says "Attach to message" instead — the promise made to the human
   * has to match what actually happens.
   */
  applyLabel?: string;
}

export function ExpandableImage({
  src,
  alt,
  className,
  frameClassName = '',
  loading = 'lazy',
  decoding = 'async',
  onError,
  onApply,
}: ExpandableImageProps) {
  const [open, setOpen] = useState(false);

  return (
    <>
      <button
        type="button"
        className={`block max-w-full cursor-zoom-in appearance-none border-0 bg-transparent p-0 text-left ${frameClassName}`}
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
      </button>
      {open && (
        <ImageViewer
          src={src}
          name={alt}
          onApply={onApply}
          onClose={() => setOpen(false)}
        />
      )}
    </>
  );
}

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
  applyLabel = 'Use edit',
}: ImageViewerProps) {
  const imageRef = useRef<HTMLImageElement | null>(null);
  const transformedRef = useRef<HTMLDivElement | null>(null);
  const zoomLabelRef = useRef<HTMLSpanElement | null>(null);
  const annotationRef = useRef<AnnotationSurface | null>(null);
  const transformRef = useRef<Transform>({ ...NO_TRANSFORM });
  const pointersRef = useRef(new Map<number, Point>());
  const gestureRef = useRef<Gesture>(null);
  const [drawing, setDrawing] = useState(false);
  const [penColor, setPenColor] = useState<PenToken>(PEN_COLORS[0].token);
  const [strokeCount, setStrokeCount] = useState(0);
  const [busy, setBusy] = useState<'copy' | 'share' | 'apply' | null>(null);
  // Probed once per open: the sheet cannot appear or disappear mid-viewer.
  const [shareAction] = useState(shareVerb);
  const [status, setStatus] = useState('');

  // The transform is written to style rather than to state: a pinch that
  // re-rendered React on every frame would stutter on exactly the devices that
  // pinch.
  const applyTransform = useCallback((next: Transform) => {
    const transform = clampTransform(next);
    transformRef.current = transform;
    if (transformedRef.current) {
      transformedRef.current.style.transform = transformCss(transform);
    }
    if (zoomLabelRef.current)
      zoomLabelRef.current.textContent = zoomLabel(transform);
  }, []);

  const resetTransform = useCallback(() => {
    pointersRef.current.clear();
    gestureRef.current = null;
    applyTransform({ ...NO_TRANSFORM });
  }, [applyTransform]);

  useEffect(() => {
    const previousOverflow = document.body.style.overflow;
    document.body.style.overflow = 'hidden';
    const keyDown = (event: KeyboardEvent) => {
      if (event.key === 'Escape') onClose();
    };
    window.addEventListener('keydown', keyDown);
    return () => {
      document.body.style.overflow = previousOverflow;
      window.removeEventListener('keydown', keyDown);
    };
  }, [onClose]);

  function fitAnnotations() {
    const image = imageRef.current;
    if (image)
      annotationRef.current?.fit(image.naturalWidth, image.naturalHeight);
  }

  function beginGesture(event: ReactPointerEvent<HTMLDivElement>) {
    if (drawing) return;
    event.preventDefault();
    event.currentTarget.setPointerCapture(event.pointerId);
    const point = { x: event.clientX, y: event.clientY };
    pointersRef.current.set(event.pointerId, point);
    const pointers = [...pointersRef.current.values()];
    const [a, b] = pointers;
    gestureRef.current =
      a && b
        ? pinchFrom(a, b, transformRef.current)
        : panFrom(event.pointerId, point, transformRef.current);
  }

  function moveGesture(event: ReactPointerEvent<HTMLDivElement>) {
    if (drawing || !pointersRef.current.has(event.pointerId)) return;
    event.preventDefault();
    pointersRef.current.set(event.pointerId, {
      x: event.clientX,
      y: event.clientY,
    });
    const gesture = gestureRef.current;
    if (!gesture) return;
    const [a, b] = [...pointersRef.current.values()];
    if (gesture.kind === 'pinch' && a && b) {
      applyTransform(pinchTransform(gesture, a, b));
    } else if (
      gesture.kind === 'pan' &&
      gesture.pointerId === event.pointerId
    ) {
      applyTransform(
        panTransform(gesture, { x: event.clientX, y: event.clientY }),
      );
    }
  }

  function endGesture(event: ReactPointerEvent<HTMLDivElement>) {
    if (drawing) return;
    pointersRef.current.delete(event.pointerId);
    // Lifting one finger of a pinch continues as a pan from where the other one
    // is, instead of jumping the picture on the next move.
    const [remaining] = [...pointersRef.current.entries()];
    gestureRef.current = remaining
      ? panFrom(remaining[0], remaining[1], transformRef.current)
      : null;
  }

  function zoomBy(factor: number) {
    applyTransform(zoomedBy(transformRef.current, factor));
  }

  function wheelZoom(event: ReactWheelEvent<HTMLDivElement>) {
    if (drawing) return;
    event.preventDefault();
    zoomBy(event.deltaY < 0 ? 1.15 : 1 / 1.15);
  }

  function toggleZoom() {
    if (drawing) return;
    if (transformRef.current.scale > 1) resetTransform();
    else applyTransform({ scale: 2, x: 0, y: 0 });
  }

  function editedImage(): Promise<Blob> {
    const image = imageRef.current;
    if (!image) throw new Error('Image is not ready');
    return flattenAnnotations(image, annotationRef.current?.canvas() ?? null);
  }

  /**
   * Every finished action is the same three steps — say what is happening, do
   * it, say how it went — so they are said once, and each caller only supplies
   * the work and the sentence to fall back to.
   */
  async function run(
    kind: 'copy' | 'share' | 'apply',
    fallback: string,
    work: (blob: Blob) => Promise<string>,
  ) {
    setBusy(kind);
    setStatus('Preparing image…');
    try {
      setStatus(await work(await editedImage()));
    } catch (cause) {
      setStatus(cause instanceof Error ? cause.message : fallback);
    } finally {
      setBusy(null);
    }
  }

  const viewer = (
    <div
      className="fixed inset-0 z-[100] isolate bg-ink text-white"
      role="dialog"
      aria-modal="true"
      aria-label={`${name} image viewer`}
    >
      <header className="absolute inset-x-0 top-0 z-20 flex min-h-14 items-center gap-3 border-b border-dialog-edge bg-panel pb-2 pl-[max(0.75rem,env(safe-area-inset-left))] pr-[max(0.75rem,env(safe-area-inset-right))] pt-[max(0.5rem,env(safe-area-inset-top))]">
        <div className="min-w-0 flex-1 truncate font-mono text-ui text-dialog-hint-key">
          {name}
        </div>
        <Button variant="ghost" className="py-2" onClick={onClose} autoFocus>
          Close
        </Button>
      </header>

      <div
        className={`absolute inset-0 grid cursor-grab place-items-center overflow-hidden overscroll-none px-4 pt-20 active:cursor-grabbing [touch-action:none] ${drawing ? 'pb-36' : 'pb-24'}`}
        onPointerDown={beginGesture}
        onPointerMove={moveGesture}
        onPointerUp={endGesture}
        onPointerCancel={endGesture}
        onDoubleClick={toggleZoom}
        onWheel={wheelZoom}
      >
        <div
          ref={transformedRef}
          className="relative inline-block origin-center transform-gpu transition-transform duration-100 motion-reduce:transition-none"
        >
          <img
            ref={imageRef}
            src={src}
            alt={name}
            draggable={false}
            onLoad={fitAnnotations}
            className={`block max-w-[calc(100vw-2rem)] select-none object-contain ${drawing ? 'max-h-[calc(100dvh-13rem)]' : 'max-h-[calc(100dvh-10rem)]'}`}
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
          <div
            className="flex shrink-0 items-center"
            aria-label="Zoom controls"
          >
            <Button
              variant="ghost"
              className="py-2"
              onClick={() => zoomBy(1 / 1.35)}
              aria-label="Zoom out"
            >
              −
            </Button>
            <Button
              variant="ghost"
              className="min-w-14 border-x-0 py-2"
              onClick={resetTransform}
              aria-label="Reset zoom"
            >
              <span ref={zoomLabelRef}>100%</span>
            </Button>
            <Button
              variant="ghost"
              className="py-2"
              onClick={() => zoomBy(1.35)}
              aria-label="Zoom in"
            >
              +
            </Button>
          </div>

          <Button
            variant={drawing ? 'solid' : 'ghost'}
            className="py-2"
            onClick={() => {
              resetTransform();
              setDrawing((current) => !current);
              setStatus('');
            }}
            aria-pressed={drawing}
          >
            {drawing ? 'Done' : 'Draw'}
          </Button>

          <div className="ml-auto flex shrink-0 items-center gap-2">
            <Button
              variant="ghost"
              className="py-2"
              onClick={() =>
                run('copy', 'Could not copy image', (blob) =>
                  copyImage(blob, name),
                )
              }
              disabled={busy !== null}
            >
              {busy === 'copy' ? 'Copying…' : 'Copy'}
            </Button>
            {/* Exactly ONE solid button is on screen at a time: when the picture can go
                back into the message, THAT is the primary action and sharing steps down. */}
            <Button
              variant={onApply ? 'ghost' : 'solid'}
              className="py-2"
              onClick={() =>
                run('share', 'Could not share image', (blob) =>
                  shareImage(blob, name),
                )
              }
              disabled={busy !== null}
            >
              {busy === 'share' ? `${shareAction}…` : shareAction}
            </Button>
            {onApply && (
              <Button
                variant="solid"
                className="py-2"
                onClick={() =>
                  // Hand the drawn-on picture back to whoever opened the viewer. The
                  // caller owns the slot it came from, so an annotated attachment
                  // REPLACES itself instead of arriving as a second copy — and the
                  // original bytes are never mutated here.
                  run('apply', 'Could not use this edit', async (blob) => {
                    await onApply(blob);
                    onClose();
                    return '';
                  })
                }
                disabled={busy !== null}
              >
                {busy === 'apply' ? 'Applying…' : applyLabel}
              </Button>
            )}
          </div>
        </div>

        {drawing && (
          <PenToolbar
            color={penColor}
            onColor={setPenColor}
            strokeCount={strokeCount}
            onUndo={() => annotationRef.current?.undo()}
            onClear={() => annotationRef.current?.clear()}
            className="mx-auto mt-1 flex max-w-[1400px] items-center justify-center gap-1 overflow-x-auto overscroll-x-contain pb-1"
          />
        )}

        <div
          className="mx-auto min-h-4 max-w-[1400px] truncate pt-1 text-center font-mono text-chip text-dialog-hint"
          aria-live="polite"
        >
          {status ||
            (drawing
              ? onApply
                ? 'Draw on the image, then use the edit in your message.'
                : 'Draw on the image, then copy or share it.'
              : 'Pinch, scroll, or double-click to zoom.')}
        </div>
      </div>
    </div>
  );

  return createPortal(viewer, document.body);
}
