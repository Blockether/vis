import {
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
  type ReactNode,
} from 'react';
import { createPortal } from 'react-dom';
import { PEN_COLORS, flattenAnnotations, type PenToken } from '../lib/annotate';
import { copyImage, shareImage, shareVerb } from '../lib/image-share';
import { partPixels, visiblePart } from '../lib/zoom-pan';
import { useZoomPan, ZOOM_FRAME_CLASS, ZOOMED_CLASS } from '../lib/zoom-surface';
import { AnnotationLayer, PenToolbar, type AnnotationSurface } from './AnnotationLayer';
import {
  CheckIcon,
  ChevronIcon,
  CopyIcon,
  DownloadIcon,
  DrawIcon,
  MinusIcon,
  PlusIcon,
  ShareIcon,
  TrimIcon,
} from './icons';
import { Button, DialogHeader, IconButton, Spinner } from './ui';
import { useGalleryStep, type GalleryPicture } from '../lib/gallery';
import { useStickyOverlay } from '../lib/sticky-overlay';

interface ExpandableImageProps {
  src: string;
  alt: string;
  className: string;
  children?: ReactNode;
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
  commentable?: boolean;
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
  commentable?: boolean;
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
  frameClassName = '',
  loading = 'lazy',
  decoding = 'async',
  onError,
  onApply,
  galleryAt,
  commentable = false,
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
    commentable,
  });

  return (
    <>
      <button
        type="button"
        className={`${
          children ? 'flex min-w-0 items-center gap-1.5' : 'block'
        } max-w-full cursor-zoom-in appearance-none border-0 bg-transparent p-0 text-left ${frameClassName}`}
        onClick={() => setOpen(true)}
        aria-label={`Open ${alt} full screen`}
      >
        <img
          src={src}
          alt={alt}
          aria-hidden={children ? true : undefined}
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
          commentable={commentable}
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
 * The viewer itself owns only the SCREEN: what to announce, which button is busy.
 * The zoom, pan and swipe come from `lib/zoom-surface`, which a PDF page shares,
 * the strokes belong to {@link AnnotationLayer}, flattening to
 * `lib/annotate` and the platform sheets to `lib/image-share` — so each of
 * those can be used, and tested, without opening a full-screen dialog.
 */
export function ImageViewer({
  src,
  name,
  onClose,
  onApply,
  applyLabel = 'Save',
  pictures,
  at,
  commentable = false,
}: ImageViewerProps) {
  const imageRef = useRef<HTMLImageElement | null>(null);
  const annotationRef = useRef<AnnotationSurface | null>(null);
  const [drawingRequested, setDrawing] = useState(false);
  const [drawingToolsOpen, setDrawingToolsOpen] = useState(true);
  const [penColor, setPenColor] = useState<PenToken>(PEN_COLORS[0].token);
  const [strokeCount, setStrokeCount] = useState(0);
  const [busy, setBusy] = useState<'copy' | 'share' | 'apply' | 'trim' | null>(null);
  // Probed once per open: the sheet cannot appear or disappear mid-viewer.
  const [shareAction] = useState(shareVerb);
  const [status, setStatus] = useState('');
  const [error, setError] = useState('');
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
  const [trimmed, setTrimmed] = useState<{ src: string; name: string } | null>(null);
  const trimUrlsRef = useRef<string[]>([]);
  const dropTrim = useCallback(() => {
    for (const url of trimUrlsRef.current) URL.revokeObjectURL(url);
    trimUrlsRef.current = [];
    setTrimmed(null);
  }, []);
  // A trim lives in memory for as long as the viewer is open and not one
  // moment longer — a closed dialog that keeps its blobs is a leak per tap.
  useEffect(() => dropTrim, [dropTrim]);
  const found = gallery ? gallery.findIndex((picture) => picture.src === shownSrc) : -1;
  const step = found < 0 ? (at ?? 0) : found;
  const untrimmed = (found < 0 ? undefined : gallery?.[found]) ?? { src, name, commentable };
  const editable = untrimmed.commentable === true || onApply !== undefined;
  const drawing = editable && drawingRequested;
  const shown = trimmed ?? untrimmed;
  const hasEdits = strokeCount > 0 || trimmed !== null;

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
      setStatus('');
      setError('');
      setShownSrc(gallery[target].src);
    },
    [gallery, drawing, step, dropTrim],
  );

  // The zoom, the pan and the swipe are the ones a PDF page takes too
  // (`lib/zoom-surface`). A new picture opens fitted.
  const zoom = useZoomPan({
    resetKey: shown.src,
    pen: drawing ? annotationRef : null,
    swipe: gallery
      ? {
          back: step > 0,
          forward: step < gallery.length - 1,
          onStep: (direction) => stepTo(step + direction),
        }
      : null,
  });

  useEffect(() => {
    const previousOverflow = document.body.style.overflow;
    document.body.style.overflow = 'hidden';
    // Left and right are what a reader already presses in front of a gallery,
    // on a phone keyboard as much as on a desktop one, so the viewer answers
    // them instead of making the reader close it once per picture.
    const keyDown = (event: KeyboardEvent) => {
      if (event.key === 'Escape') onClose();
      if (event.key === 'ArrowLeft') stepTo(step - 1);
      if (event.key === 'ArrowRight') stepTo(step + 1);
    };
    window.addEventListener('keydown', keyDown);
    return () => {
      document.body.style.overflow = previousOverflow;
      window.removeEventListener('keydown', keyDown);
    };
  }, [onClose, stepTo, step]);

  function fitAnnotations() {
    const image = imageRef.current;
    if (image) annotationRef.current?.fit(image.naturalWidth, image.naturalHeight);
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
    const frame = zoom.viewportRef.current;
    if (!image || !frame) return;
    const part = visiblePart(image.getBoundingClientRect(), frame.getBoundingClientRect());
    if (!part) {
      setStatus('');
      setError('Zoom in first — the whole picture is already in view.');
      return;
    }
    setBusy('trim');
    setError('');
    setStatus('Trimming…');
    try {
      const blob = await flattenAnnotations(image, annotationRef.current?.canvas() ?? null, part);
      const url = URL.createObjectURL(blob);
      trimUrlsRef.current.push(url);
      const size = partPixels(part, image.naturalWidth, image.naturalHeight);
      // The strokes went INTO the trim; left on the layer they would be
      // painted a second time, stretched across the pixels that survived.
      annotationRef.current?.clear();
      zoom.reset();
      setTrimmed({ src: url, name: shown.name });
      setStatus(`Trimmed to ${size.width} × ${size.height}.`);
    } catch (cause) {
      setStatus('');
      setError(cause instanceof Error ? cause.message : 'Could not trim this image');
    } finally {
      setBusy(null);
    }
  }

  function editedImage(): Promise<Blob> {
    const image = imageRef.current;
    if (!image) throw new Error('Image is not ready');
    return flattenAnnotations(image, annotationRef.current?.canvas() ?? null);
  }

  /** Announce completed work without leaving a visible line below the toolbar. */
  async function run(
    kind: 'copy' | 'share' | 'apply',
    fallback: string,
    work: (blob: Blob) => Promise<string>,
  ) {
    setBusy(kind);
    setStatus('');
    setError('');
    try {
      setStatus(await work(await editedImage()));
    } catch (cause) {
      setError(cause instanceof Error ? cause.message : fallback);
    } finally {
      setBusy(null);
    }
  }

  /**
   * Hand the edited picture back to whoever opened the viewer, and leave. The
   * caller owns the slot it came from, so an annotated attachment REPLACES itself
   * instead of arriving as a second copy, and the original bytes are never mutated
   * here. The footer check calls this from the pencil's own slot: accepting a drawing
   * changes the tool in place instead of adding a detached verb to the title band.
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
    await run('apply', 'Could not save this edit', async (blob) => {
      await onApply(blob);
      onClose();
      return '';
    });
  }

  const viewer = (
    <div
      className="fixed inset-0 z-[100] isolate bg-ink text-white"
      role="dialog"
      aria-modal="true"
      aria-label={`${shown.name} image viewer`}
    >
      {/* The picture's own title bar is the app's one dialog band. It floats over
          the image, clears the notch, and carries only the title and the way out.
          Drawing is accepted where it started: the pencil's footer slot changes to
          a check while the pen is active. */}
      <DialogHeader
        title={shown.name}
        isTitleTruncated
        closeLabel={`Close ${shown.name}`}
        onClose={onClose}
        isUnderNotch
        className="absolute inset-x-0 top-0 z-20"
      />

      {editable && drawing && (
        <div className="absolute right-[max(0.75rem,env(safe-area-inset-right))] top-1/2 z-20 flex -translate-y-1/2 items-center gap-1 sm:right-4">
          <IconButton
            variant="overlay"
            label={drawingToolsOpen ? 'Hide drawing tools' : 'Show drawing tools'}
            title={drawingToolsOpen ? 'Hide drawing tools' : 'Show drawing tools'}
            aria-expanded={drawingToolsOpen}
            onClick={() => setDrawingToolsOpen((open) => !open)}
          >
            <ChevronIcon back={!drawingToolsOpen} />
          </IconButton>
          {drawingToolsOpen && (
            <PenToolbar
              color={penColor}
              onColor={setPenColor}
              strokeCount={strokeCount}
              onUndo={() => annotationRef.current?.undo()}
              onClear={() => annotationRef.current?.clear()}
            />
          )}
        </div>
      )}

      <div className="sr-only" aria-live="polite">
        {status || (gallery ? `${step + 1} of ${gallery.length}` : '')}
      </div>
      {error && (
        <div
          role="alert"
          className="absolute inset-x-4 top-[calc(4rem+env(safe-area-inset-top))] z-20 mx-auto max-w-lg border border-warn-edge bg-warn-surface px-3 py-2 text-center font-mono text-chip text-answer-foreground shadow-float"
        >
          {error}
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
        className={`absolute inset-0 grid cursor-grab place-items-center px-4 pb-24 pt-[calc(5rem+env(safe-area-inset-top))] active:cursor-grabbing sm:pt-20 ${ZOOM_FRAME_CLASS}`}
        {...zoom.handlers}
        ref={zoom.viewportRef}
      >
        <div
          ref={zoom.transformedRef}
          className={`relative inline-block ${ZOOMED_CLASS}`}
        >
          <img
            ref={imageRef}
            src={shown.src}
            alt={shown.name}
            draggable={false}
            onLoad={fitAnnotations}
            className="block max-h-full max-w-[calc(100vw-2rem)] select-none object-contain"
          />
          {editable ? (
            <AnnotationLayer
              ref={annotationRef}
              active={drawing}
              color={penColor}
              onStrokesChange={setStrokeCount}
              label="Image annotation layer"
              className="absolute inset-0 size-full"
            />
          ) : null}
        </div>
      </div>

      <div className="absolute inset-x-0 bottom-0 z-20 border-t border-dialog-edge bg-panel pb-[max(0.75rem,env(safe-area-inset-bottom))] pl-[max(0.75rem,env(safe-area-inset-left))] pr-[max(0.75rem,env(safe-area-inset-right))] pt-2">
        <div className="mx-auto flex max-w-[1400px] items-center gap-5 overflow-x-auto overscroll-x-contain p-1.5 mouse:gap-2 mouse:p-0 mouse:pb-1">
          <div
            className="flex shrink-0 items-center gap-3.5 mouse:gap-2"
            role="group"
            aria-label="Zoom controls"
          >
            <IconButton
              variant="secondary"
              onClick={() => zoom.zoomBy(1 / 1.35)}
              label="Zoom out"
              title="Zoom out"
            >
              <MinusIcon className="size-3" />
            </IconButton>
            <Button
              variant="secondary"
              className="min-w-14"
              onClick={zoom.reset}
              aria-label="Reset zoom"
            >
              <span ref={zoom.labelRef}>100%</span>
            </Button>
            <IconButton
              variant="secondary"
              onClick={() => zoom.zoomBy(1.35)}
              label="Zoom in"
              title="Zoom in"
            >
              <PlusIcon className="size-3" />
            </IconButton>
          </div>

          {gallery && (
            <span className="shrink-0 font-mono text-chip text-dialog-hint" aria-hidden="true">
              {step + 1} of {gallery.length}
            </span>
          )}

          {editable ? (
            <IconButton
              variant={onApply && hasEdits ? 'primary' : 'secondary'}
              label={
                onApply && (drawing || hasEdits)
                  ? applyLabel === 'Save'
                    ? busy === 'apply'
                      ? 'Saving changes'
                      : 'Save changes'
                    : applyLabel
                  : drawing
                    ? 'Finish drawing'
                    : 'Draw on image'
              }
              title={
                onApply && (drawing || hasEdits)
                  ? applyLabel === 'Save'
                    ? 'Save changes'
                    : applyLabel
                  : drawing
                    ? 'Finish drawing'
                    : 'Draw on image'
              }
              onClick={() => {
                if (onApply && (drawing || hasEdits)) {
                  void applyEdit();
                  return;
                }
                zoom.reset();
                setDrawing((current) => !current);
                setStatus('');
                setError('');
              }}
              aria-pressed={drawing}
              disabled={busy !== null}
            >
              {busy === 'apply' ? (
                <Spinner />
              ) : drawing || (onApply && hasEdits) ? (
                <CheckIcon />
              ) : (
                <DrawIcon />
              )}
            </IconButton>
          ) : null}

          {editable ? (
            <IconButton
              variant="secondary"
              label="Trim to view"
              title="Trim to view"
              onClick={trimToView}
              disabled={drawing || busy !== null}
            >
              <TrimIcon />
            </IconButton>
          ) : null}

          {trimmed && (
            <Button
              variant="secondary"
              onClick={() => {
                annotationRef.current?.clear();
                dropTrim();
                zoom.reset();
                setStatus('');
                setError('');
              }}
              disabled={busy !== null}
              aria-label="Undo trim"
            >
              Undo trim
            </Button>
          )}

          <div className="ml-auto flex shrink-0 items-center gap-5 mouse:gap-2">
            <IconButton
              variant="secondary"
              label="Copy image"
              title="Copy image"
              onClick={() =>
                run('copy', 'Could not copy image', (blob) => copyImage(blob, shown.name))
              }
              disabled={busy !== null}
            >
              <CopyIcon />
            </IconButton>
            {/* The check becomes the primary only after there is an edit to keep;
                until then Share remains the strip's one filled verb. */}
            <IconButton
              variant={onApply && hasEdits ? 'secondary' : 'primary'}
              label={`${shareAction} image`}
              title={`${shareAction} image`}
              onClick={() =>
                run('share', 'Could not share image', (blob) => shareImage(blob, shown.name))
              }
              disabled={busy !== null}
            >
              {shareAction === 'Share' ? <ShareIcon /> : <DownloadIcon />}
            </IconButton>
          </div>
        </div>
      </div>
    </div>
  );

  return createPortal(viewer, document.body);
}
