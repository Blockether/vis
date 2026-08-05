import { Clipboard } from '@capacitor/clipboard';
import { Capacitor } from '@capacitor/core';
import { Directory, Filesystem } from '@capacitor/filesystem';
import { Share } from '@capacitor/share';
import {
  useCallback,
  useEffect,
  useRef,
  useState,
  type PointerEvent as ReactPointerEvent,
  type WheelEvent as ReactWheelEvent,
} from 'react';
import { createPortal } from 'react-dom';
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

type Point = { x: number; y: number };
type Stroke = { color: string; width: number; points: Point[] };
type Transform = { scale: number; x: number; y: number };

type Gesture =
  | { kind: 'pan'; pointerId: number; start: Point; transform: Transform }
  | { kind: 'pinch'; distance: number; midpoint: Point; transform: Transform }
  | null;

const MIN_SCALE = 1;
const MAX_SCALE = 6;
// Pen colours are THEME colours, named by palette variable rather than frozen
// as hex: `/v1/theme` repaints the whole app from the gateway, and an ink that
// ignores it is the one mark on the picture that belongs to another product.
// The canvas needs a real colour string, so the value is read from the live
// palette at the moment a stroke starts.
const PEN_COLORS = [
  { token: '--err', className: 'bg-err', label: 'Red pen' },
  { token: '--fg', className: 'bg-white', label: 'Ink pen' },
  { token: '--primary', className: 'bg-accent', label: 'Amber pen' },
  { token: '--link-fg', className: 'bg-link', label: 'Blue pen' },
  { token: '--ok', className: 'bg-ok', label: 'Green pen' },
] as const;

/** The live value behind a palette variable — canvas work cannot use a class. */
function paletteColor(token: string): string {
  const value = getComputedStyle(document.documentElement).getPropertyValue(token).trim();
  return value || '#ef4444';
}

/**
 * Does a real share sheet exist for image FILES?
 *
 * Without one the share button downloads, and calling that "Share" is a lie the
 * user only discovers after tapping — so the label follows the capability.
 */
function canShareImageFiles(): boolean {
  if (Capacitor.isNativePlatform()) return true;
  if (typeof navigator.share !== 'function') return false;
  if (typeof navigator.canShare !== 'function') return true;
  return navigator.canShare({ files: [new File([], 'image.png', { type: 'image/png' })] });
}

function distance(a: Point, b: Point): number {
  return Math.hypot(b.x - a.x, b.y - a.y);
}

function midpoint(a: Point, b: Point): Point {
  return { x: (a.x + b.x) / 2, y: (a.y + b.y) / 2 };
}

function clamp(value: number, min: number, max: number): number {
  return Math.min(max, Math.max(min, value));
}

function blobAsDataUrl(blob: Blob): Promise<string> {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onload = () => resolve(String(reader.result));
    reader.onerror = () => reject(reader.error ?? new Error('Could not read image'));
    reader.readAsDataURL(blob);
  });
}

function canvasAsBlob(canvas: HTMLCanvasElement): Promise<Blob> {
  return new Promise((resolve, reject) => {
    canvas.toBlob((blob) => {
      if (blob) resolve(blob);
      else reject(new Error('Could not prepare image'));
    }, 'image/png');
  });
}

function shareFilename(name: string): string {
  const base = name.replace(/\.[^.]+$/u, '').replace(/[^a-z0-9._-]+/giu, '-').replace(/^-+|-+$/gu, '');
  return `${base || 'vis-image'}.png`;
}

function actionCancelled(cause: unknown): boolean {
  return /cancel|dismiss|abort/iu.test(cause instanceof Error ? cause.message : String(cause));
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
        <ImageViewer src={src} name={alt} onApply={onApply} onClose={() => setOpen(false)} />
      )}
    </>
  );
}

/**
 * A picture, full screen: pinch/scroll/double-click zoom, pan, and a pen. Given
 * `onApply` it also hands the flattened result back, which is what makes an
 * attachment editable and a rendered document page sendable.
 */
export function ImageViewer({ src, name, onClose, onApply, applyLabel = 'Use edit' }: ImageViewerProps) {
  const imageRef = useRef<HTMLImageElement | null>(null);
  const transformedRef = useRef<HTMLDivElement | null>(null);
  const canvasRef = useRef<HTMLCanvasElement | null>(null);
  const zoomLabelRef = useRef<HTMLSpanElement | null>(null);
  const transformRef = useRef<Transform>({ scale: 1, x: 0, y: 0 });
  const pointersRef = useRef(new Map<number, Point>());
  const gestureRef = useRef<Gesture>(null);
  const strokesRef = useRef<Stroke[]>([]);
  const activeStrokeRef = useRef<Stroke | null>(null);
  const [drawing, setDrawing] = useState(false);
  const [penColor, setPenColor] = useState<string>(PEN_COLORS[0].token);
  const [strokeCount, setStrokeCount] = useState(0);
  const [busy, setBusy] = useState<'copy' | 'share' | 'apply' | null>(null);
  // Probed once per open: the sheet cannot appear or disappear mid-viewer.
  const [shareVerb] = useState(() => (canShareImageFiles() ? 'Share' : 'Save'));
  const [status, setStatus] = useState('');

  const applyTransform = useCallback((next: Transform) => {
    const scale = clamp(next.scale, MIN_SCALE, MAX_SCALE);
    const transform = scale === 1 ? { scale, x: 0, y: 0 } : { scale, x: next.x, y: next.y };
    transformRef.current = transform;
    if (transformedRef.current) {
      transformedRef.current.style.transform = `translate3d(${transform.x}px, ${transform.y}px, 0) scale(${transform.scale})`;
    }
    if (zoomLabelRef.current) zoomLabelRef.current.textContent = `${Math.round(scale * 100)}%`;
  }, []);

  const resetTransform = useCallback(() => {
    pointersRef.current.clear();
    gestureRef.current = null;
    applyTransform({ scale: 1, x: 0, y: 0 });
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

  function syncCanvasSize() {
    const image = imageRef.current;
    const canvas = canvasRef.current;
    if (!image || !canvas || !image.naturalWidth || !image.naturalHeight) return;
    if (canvas.width === image.naturalWidth && canvas.height === image.naturalHeight) return;
    canvas.width = image.naturalWidth;
    canvas.height = image.naturalHeight;
    strokesRef.current = [];
    activeStrokeRef.current = null;
    setStrokeCount(0);
  }

  function redrawStrokes() {
    const canvas = canvasRef.current;
    const context = canvas?.getContext('2d');
    if (!canvas || !context) return;
    context.clearRect(0, 0, canvas.width, canvas.height);
    for (const stroke of strokesRef.current) {
      const first = stroke.points[0];
      if (!first) continue;
      context.strokeStyle = stroke.color;
      context.fillStyle = stroke.color;
      context.lineWidth = stroke.width;
      context.lineCap = 'round';
      context.lineJoin = 'round';
      if (stroke.points.length === 1) {
        context.beginPath();
        context.arc(first.x, first.y, stroke.width / 2, 0, Math.PI * 2);
        context.fill();
        continue;
      }
      context.beginPath();
      context.moveTo(first.x, first.y);
      for (const point of stroke.points.slice(1)) context.lineTo(point.x, point.y);
      context.stroke();
    }
  }

  function canvasPoint(event: ReactPointerEvent<HTMLCanvasElement>): Point | null {
    const canvas = canvasRef.current;
    if (!canvas) return null;
    const rect = canvas.getBoundingClientRect();
    if (!rect.width || !rect.height) return null;
    return {
      x: ((event.clientX - rect.left) / rect.width) * canvas.width,
      y: ((event.clientY - rect.top) / rect.height) * canvas.height,
    };
  }

  function startStroke(event: ReactPointerEvent<HTMLCanvasElement>) {
    if (!drawing || !event.isPrimary) return;
    const canvas = canvasRef.current;
    const point = canvasPoint(event);
    if (!canvas || !point) return;
    event.preventDefault();
    event.stopPropagation();
    event.currentTarget.setPointerCapture(event.pointerId);
    const stroke: Stroke = {
      color: paletteColor(penColor),
      width: Math.max(3, Math.max(canvas.width, canvas.height) * 0.006),
      points: [point],
    };
    strokesRef.current.push(stroke);
    activeStrokeRef.current = stroke;
    redrawStrokes();
  }

  function continueStroke(event: ReactPointerEvent<HTMLCanvasElement>) {
    const stroke = activeStrokeRef.current;
    const canvas = canvasRef.current;
    const point = canvasPoint(event);
    if (!drawing || !stroke || !canvas || !point) return;
    event.preventDefault();
    event.stopPropagation();
    const previous = stroke.points.at(-1);
    stroke.points.push(point);
    if (!previous) return;
    const context = canvas.getContext('2d');
    if (!context) return;
    context.strokeStyle = stroke.color;
    context.lineWidth = stroke.width;
    context.lineCap = 'round';
    context.lineJoin = 'round';
    context.beginPath();
    context.moveTo(previous.x, previous.y);
    context.lineTo(point.x, point.y);
    context.stroke();
  }

  function finishStroke(event: ReactPointerEvent<HTMLCanvasElement>) {
    if (!activeStrokeRef.current) return;
    event.preventDefault();
    event.stopPropagation();
    activeStrokeRef.current = null;
    setStrokeCount(strokesRef.current.length);
  }

  function undoStroke() {
    strokesRef.current.pop();
    activeStrokeRef.current = null;
    setStrokeCount(strokesRef.current.length);
    redrawStrokes();
  }

  function clearStrokes() {
    strokesRef.current = [];
    activeStrokeRef.current = null;
    setStrokeCount(0);
    redrawStrokes();
  }

  function beginGesture(event: ReactPointerEvent<HTMLDivElement>) {
    if (drawing) return;
    event.preventDefault();
    event.currentTarget.setPointerCapture(event.pointerId);
    const point = { x: event.clientX, y: event.clientY };
    pointersRef.current.set(event.pointerId, point);
    const pointers = [...pointersRef.current.entries()];
    if (pointers.length >= 2) {
      const a = pointers[0]?.[1];
      const b = pointers[1]?.[1];
      if (!a || !b) return;
      gestureRef.current = {
        kind: 'pinch',
        distance: Math.max(1, distance(a, b)),
        midpoint: midpoint(a, b),
        transform: { ...transformRef.current },
      };
    } else {
      gestureRef.current = {
        kind: 'pan',
        pointerId: event.pointerId,
        start: point,
        transform: { ...transformRef.current },
      };
    }
  }

  function moveGesture(event: ReactPointerEvent<HTMLDivElement>) {
    if (drawing || !pointersRef.current.has(event.pointerId)) return;
    event.preventDefault();
    pointersRef.current.set(event.pointerId, { x: event.clientX, y: event.clientY });
    const gesture = gestureRef.current;
    if (!gesture) return;
    const pointers = [...pointersRef.current.values()];
    if (gesture.kind === 'pinch' && pointers.length >= 2) {
      const a = pointers[0];
      const b = pointers[1];
      if (!a || !b) return;
      const center = midpoint(a, b);
      applyTransform({
        scale: gesture.transform.scale * (distance(a, b) / gesture.distance),
        x: gesture.transform.x + center.x - gesture.midpoint.x,
        y: gesture.transform.y + center.y - gesture.midpoint.y,
      });
    } else if (gesture.kind === 'pan' && gesture.pointerId === event.pointerId) {
      applyTransform({
        scale: gesture.transform.scale,
        x: gesture.transform.x + event.clientX - gesture.start.x,
        y: gesture.transform.y + event.clientY - gesture.start.y,
      });
    }
  }

  function endGesture(event: ReactPointerEvent<HTMLDivElement>) {
    if (drawing) return;
    pointersRef.current.delete(event.pointerId);
    const remaining = [...pointersRef.current.entries()];
    if (remaining.length === 1) {
      const [pointerId, point] = remaining[0] ?? [];
      if (pointerId !== undefined && point) {
        gestureRef.current = {
          kind: 'pan',
          pointerId,
          start: point,
          transform: { ...transformRef.current },
        };
      }
    } else {
      gestureRef.current = null;
    }
  }

  function zoomBy(factor: number) {
    const current = transformRef.current;
    applyTransform({ ...current, scale: current.scale * factor });
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

  async function editedImage(): Promise<Blob> {
    const image = imageRef.current;
    if (!image?.naturalWidth || !image.naturalHeight) throw new Error('Image is not ready');
    const output = document.createElement('canvas');
    output.width = image.naturalWidth;
    output.height = image.naturalHeight;
    const context = output.getContext('2d');
    if (!context) throw new Error('Image editing is unavailable');
    context.drawImage(image, 0, 0, output.width, output.height);
    const annotations = canvasRef.current;
    if (annotations?.width && annotations.height) {
      context.drawImage(annotations, 0, 0, output.width, output.height);
    }
    return canvasAsBlob(output);
  }

  async function copyImage() {
    setBusy('copy');
    setStatus('Preparing image...');
    try {
      const blob = await editedImage();
      if (Capacitor.isNativePlatform()) {
        await Clipboard.write({ image: await blobAsDataUrl(blob), label: name });
      } else if (navigator.clipboard?.write && typeof ClipboardItem !== 'undefined') {
        await navigator.clipboard.write([new ClipboardItem({ 'image/png': blob })]);
      } else {
        throw new Error('Image copying is not supported by this browser');
      }
      setStatus('Image copied. Paste it into your next message.');
    } catch (cause) {
      setStatus(cause instanceof Error ? cause.message : 'Could not copy image');
    } finally {
      setBusy(null);
    }
  }

  async function shareImage() {
    setBusy('share');
    setStatus('Preparing image...');
    let nativePath: string | null = null;
    try {
      const blob = await editedImage();
      const filename = shareFilename(name);
      if (Capacitor.isNativePlatform()) {
        nativePath = `shared/${Date.now()}-${filename}`;
        const dataUrl = await blobAsDataUrl(blob);
        await Filesystem.writeFile({
          path: nativePath,
          directory: Directory.Cache,
          data: dataUrl.slice(dataUrl.indexOf(',') + 1),
          recursive: true,
        });
        const { uri } = await Filesystem.getUri({ path: nativePath, directory: Directory.Cache });
        await Share.share({ title: name, files: [uri], dialogTitle: 'Share image' });
        setStatus('Image shared.');
      } else {
        const file = new File([blob], filename, { type: 'image/png' });
        if (navigator.share && (!navigator.canShare || navigator.canShare({ files: [file] }))) {
          await navigator.share({ title: name, files: [file] });
          setStatus('Image shared.');
        } else {
          const url = URL.createObjectURL(blob);
          const link = document.createElement('a');
          link.href = url;
          link.download = filename;
          link.click();
          window.setTimeout(() => URL.revokeObjectURL(url), 1_000);
          setStatus('Image downloaded.');
        }
      }
    } catch (cause) {
      if (!actionCancelled(cause)) {
        setStatus(cause instanceof Error ? cause.message : 'Could not share image');
      } else {
        setStatus('');
      }
    } finally {
      if (nativePath) {
        void Filesystem.deleteFile({ path: nativePath, directory: Directory.Cache }).catch(() => undefined);
      }
      setBusy(null);
    }
  }

  // Hand the drawn-on picture back to whoever opened the viewer. The caller owns
  // the slot it came from, so an annotated attachment REPLACES itself instead of
  // arriving as a second copy — and the original bytes are never mutated here.
  async function applyEdit() {
    if (!onApply) return;
    setBusy('apply');
    setStatus('Preparing image…');
    try {
      await onApply(await editedImage());
      onClose();
    } catch (cause) {
      setStatus(cause instanceof Error ? cause.message : 'Could not use this edit');
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
        <div className="min-w-0 flex-1 truncate font-mono text-ui text-dialog-hint-key">{name}</div>
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
            onLoad={syncCanvasSize}
            className={`block max-w-[calc(100vw-2rem)] select-none object-contain ${drawing ? 'max-h-[calc(100dvh-13rem)]' : 'max-h-[calc(100dvh-10rem)]'}`}
          />
          <canvas
            ref={canvasRef}
            className={`absolute inset-0 size-full [touch-action:none] ${drawing ? 'cursor-crosshair pointer-events-auto' : 'pointer-events-none'}`}
            aria-label="Image annotation layer"
            onPointerDown={startStroke}
            onPointerMove={continueStroke}
            onPointerUp={finishStroke}
            onPointerCancel={finishStroke}
          />
        </div>
      </div>

      <div className="absolute inset-x-0 bottom-0 z-20 border-t border-dialog-edge bg-panel pb-[max(0.75rem,env(safe-area-inset-bottom))] pl-[max(0.75rem,env(safe-area-inset-left))] pr-[max(0.75rem,env(safe-area-inset-right))] pt-2">
        <div className="mx-auto flex max-w-[1400px] items-center gap-2 overflow-x-auto overscroll-x-contain pb-1">
          <div className="flex shrink-0 items-center" aria-label="Zoom controls">
            <Button variant="ghost" className="py-2" onClick={() => zoomBy(1 / 1.35)} aria-label="Zoom out">
              −
            </Button>
            <Button variant="ghost" className="min-w-14 border-x-0 py-2" onClick={resetTransform} aria-label="Reset zoom">
              <span ref={zoomLabelRef}>100%</span>
            </Button>
            <Button variant="ghost" className="py-2" onClick={() => zoomBy(1.35)} aria-label="Zoom in">
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
            <Button variant="ghost" className="py-2" onClick={copyImage} disabled={busy !== null}>
              {busy === 'copy' ? 'Copying…' : 'Copy'}
            </Button>
            {/* Exactly ONE solid button is on screen at a time: when the picture can go
                back into the message, THAT is the primary action and sharing steps down. */}
            <Button
              variant={onApply ? 'ghost' : 'solid'}
              className="py-2"
              onClick={shareImage}
              disabled={busy !== null}
            >
              {busy === 'share' ? `${shareVerb}…` : shareVerb}
            </Button>
            {onApply && (
              <Button variant="solid" className="py-2" onClick={applyEdit} disabled={busy !== null}>
                {busy === 'apply' ? 'Applying…' : applyLabel}
              </Button>
            )}
          </div>
        </div>

        {drawing && (
          <div className="mx-auto mt-1 flex max-w-[1400px] items-center justify-center gap-1 overflow-x-auto overscroll-x-contain pb-1" aria-label="Drawing tools">
            {PEN_COLORS.map((pen) => (
              <button
                key={pen.token}
                type="button"
                className={`size-7 shrink-0 border-2 ${pen.className} ${penColor === pen.token ? 'border-accent' : 'border-edge-strong'}`}
                onClick={() => setPenColor(pen.token)}
                aria-label={pen.label}
                aria-pressed={penColor === pen.token}
              />
            ))}
            <Button variant="ghost" className="py-2" onClick={undoStroke} disabled={!strokeCount}>
              Undo
            </Button>
            <Button variant="ghost" className="py-2" onClick={clearStrokes} disabled={!strokeCount}>
              Clear
            </Button>
          </div>
        )}

        <div className="mx-auto min-h-4 max-w-[1400px] truncate pt-1 text-center font-mono text-chip text-dialog-hint" aria-live="polite">
          {status
            || (drawing
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
