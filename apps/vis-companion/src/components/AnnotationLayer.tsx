import {
  useEffect,
  useImperativeHandle,
  useRef,
  type PointerEvent as ReactPointerEvent,
  type Ref,
} from 'react';
import {
  PEN_COLORS,
  canvasPoint,
  paintSegment,
  paintStroke,
  paletteColor,
  repaintStrokes,
  strokeWidthFor,
  type PenToken,
  type Stroke,
} from '../lib/annotate';
import { Button } from './ui';

/**
 * A transparent sheet you can draw on, and the tools that drive it.
 *
 * The layer owns ONE thing: the strokes on this picture. It does not know what
 * is underneath it, whether that picture is zoomed, where it came from, or
 * where the result is going — which is exactly why a PDF page, an HTML capture
 * and a plain attachment can all be annotated by mounting the same component
 * over them. Strokes live in refs and are painted straight onto the canvas,
 * because a pen that re-renders React on every pointer move drags behind the
 * finger; the only thing React hears about is how MANY strokes exist, which is
 * what Undo, Clear and Apply are enabled by.
 */

/** What an owner may ask of the layer it mounted. */
export interface AnnotationSurface {
  /** Match a picture's own pixel grid. Resizing a canvas erases it, so this clears. */
  fit(width: number, height: number): void;
  /** Drop the last stroke. */
  undo(): void;
  /** Drop every stroke. */
  clear(): void;
  /** The painted sheet, to compose over the picture it annotates. */
  canvas(): HTMLCanvasElement | null;
}

export function AnnotationLayer({
  ref,
  active,
  color,
  className = '',
  label = 'Drawing layer',
  onStrokesChange,
}: {
  ref?: Ref<AnnotationSurface>;
  /** Off, the sheet is inert and every pointer reaches the picture below it. */
  active: boolean;
  color: PenToken;
  className?: string;
  label?: string;
  /** How many strokes exist now — an owner enables Undo/Clear/Apply with it. */
  onStrokesChange?: (count: number) => void;
}) {
  const canvasRef = useRef<HTMLCanvasElement | null>(null);
  const strokesRef = useRef<Stroke[]>([]);
  const drawingRef = useRef<Stroke | null>(null);
  const changed = useRef(onStrokesChange);
  useEffect(() => {
    changed.current = onStrokesChange;
  });

  function announce(): void {
    changed.current?.(strokesRef.current.length);
  }

  function repaint(): void {
    const canvas = canvasRef.current;
    const context = canvas?.getContext('2d');
    if (canvas && context) repaintStrokes(context, canvas, strokesRef.current);
    announce();
  }

  useImperativeHandle(
    ref,
    (): AnnotationSurface => ({
      fit(width, height) {
        const canvas = canvasRef.current;
        if (!canvas || !width || !height) return;
        if (canvas.width === width && canvas.height === height) return;
        canvas.width = width;
        canvas.height = height;
        strokesRef.current = [];
        drawingRef.current = null;
        announce();
      },
      undo() {
        strokesRef.current = strokesRef.current.slice(0, -1);
        drawingRef.current = null;
        repaint();
      },
      clear() {
        strokesRef.current = [];
        drawingRef.current = null;
        repaint();
      },
      canvas: () => canvasRef.current,
    }),
    [],
  );

  function begin(event: ReactPointerEvent<HTMLCanvasElement>): void {
    const canvas = canvasRef.current;
    if (!active || !event.isPrimary || !canvas) return;
    const point = canvasPoint(canvas, event.clientX, event.clientY);
    if (!point) return;
    event.preventDefault();
    event.stopPropagation();
    canvas.setPointerCapture(event.pointerId);
    const stroke: Stroke = {
      color: paletteColor(color),
      width: strokeWidthFor(canvas.width, canvas.height),
      points: [point],
    };
    strokesRef.current = [...strokesRef.current, stroke];
    drawingRef.current = stroke;
    const context = canvas.getContext('2d');
    if (context) paintStroke(context, stroke);
    announce();
  }

  function extend(event: ReactPointerEvent<HTMLCanvasElement>): void {
    const canvas = canvasRef.current;
    const stroke = drawingRef.current;
    if (!active || !stroke || !canvas) return;
    const point = canvasPoint(canvas, event.clientX, event.clientY);
    if (!point) return;
    event.preventDefault();
    event.stopPropagation();
    const previous = stroke.points[stroke.points.length - 1];
    stroke.points.push(point);
    const context = canvas.getContext('2d');
    if (previous && context) paintSegment(context, stroke, previous, point);
  }

  function finish(event: ReactPointerEvent<HTMLCanvasElement>): void {
    if (!drawingRef.current) return;
    event.preventDefault();
    event.stopPropagation();
    drawingRef.current = null;
  }

  return (
    <canvas
      ref={canvasRef}
      aria-label={label}
      data-annotation={active ? 'active' : 'idle'}
      className={`[touch-action:none] ${active ? 'pointer-events-auto cursor-crosshair' : 'pointer-events-none'} ${className}`}
      onPointerDown={begin}
      onPointerMove={extend}
      onPointerUp={finish}
      onPointerCancel={finish}
    />
  );
}

/**
 * The pen's own controls: which ink, and the two ways back. Presentational — it
 * holds no strokes, so the same strip drives a viewer, a document page, or
 * anything else that mounts an {@link AnnotationLayer}. The swatch is a 28px
 * chip inside a full-size hit box: what you see stays small, what you hit does
 * not, on a phone and on a tablet alike.
 */
export function PenToolbar({
  color,
  onColor,
  strokeCount,
  onUndo,
  onClear,
  className = '',
}: {
  color: PenToken;
  onColor: (token: PenToken) => void;
  strokeCount: number;
  onUndo: () => void;
  onClear: () => void;
  className?: string;
}) {
  return (
    <div role="group" aria-label="Drawing tools" className={className}>
      {PEN_COLORS.map((pen) => (
        <button
          key={pen.token}
          type="button"
          className="flex min-h-11 min-w-11 shrink-0 items-center justify-center"
          onClick={() => onColor(pen.token)}
          aria-label={pen.label}
          aria-pressed={color === pen.token}
        >
          <span
            className={`size-7 border-2 ${pen.className} ${
              color === pen.token ? 'border-accent' : 'border-edge-strong'
            }`}
          />
        </button>
      ))}
      <Button
        variant="ghost"
        className="py-2"
        onClick={onUndo}
        disabled={!strokeCount}
      >
        Undo
      </Button>
      <Button
        variant="ghost"
        className="py-2"
        onClick={onClear}
        disabled={!strokeCount}
      >
        Clear
      </Button>
    </div>
  );
}
