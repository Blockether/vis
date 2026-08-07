import {
  useEffect,
  useImperativeHandle,
  useRef,
  type PointerEvent as ReactPointerEvent,
  type Ref,
} from "react";
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
} from "../lib/annotate";
import { Button } from "./ui";

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
  /**
   * Abandon the stroke in progress. A second finger landed, so what looked like
   * a pen was the start of a pinch: the marks it left are not wanted.
   */
  cancelStroke(): void;
  /**
   * Draw from anywhere on screen, in client coordinates — the picture's own
   * margin included. A stroke that starts beside the sheet and crosses onto it
   * paints from the moment it arrives, instead of needing a first touch that
   * lands exactly inside the edge.
   */
  beginAt(clientX: number, clientY: number): void;
  extendTo(clientX: number, clientY: number): void;
  endStroke(): void;
  /** The painted sheet, to compose over the picture it annotates. */
  canvas(): HTMLCanvasElement | null;
}

export function AnnotationLayer({
  ref,
  active,
  color,
  className = "",
  label = "Drawing layer",
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
  // Read by the imperative surface, which is built once: a pointer that arrives
  // from an owner must use the pen this render holds, not the first one.
  const activeRef = useRef(active);
  const colorRef = useRef(color);
  useEffect(() => {
    changed.current = onStrokesChange;
    activeRef.current = active;
    colorRef.current = color;
  });

  function announce(): void {
    changed.current?.(strokesRef.current.length);
  }

  function repaint(): void {
    const canvas = canvasRef.current;
    const context = canvas?.getContext("2d");
    if (canvas && context) repaintStrokes(context, canvas, strokesRef.current);
    announce();
  }

  /**
   * The three moves of a pen, in client coordinates and nothing else, so the
   * sheet's own pointer handlers and an owner forwarding a gesture that started
   * off the sheet run the SAME code. Each answers whether it did anything.
   */
  function startStroke(clientX: number, clientY: number): boolean {
    const canvas = canvasRef.current;
    if (!activeRef.current || drawingRef.current || !canvas) return false;
    const point = canvasPoint(canvas, clientX, clientY);
    if (!point) return false;
    const stroke: Stroke = {
      color: paletteColor(colorRef.current),
      width: strokeWidthFor(canvas.width, canvas.height),
      points: [point],
    };
    strokesRef.current = [...strokesRef.current, stroke];
    drawingRef.current = stroke;
    const context = canvas.getContext("2d");
    if (context) paintStroke(context, stroke);
    announce();
    return true;
  }

  function continueStroke(clientX: number, clientY: number): boolean {
    const canvas = canvasRef.current;
    const stroke = drawingRef.current;
    if (!activeRef.current || !stroke || !canvas) return false;
    const point = canvasPoint(canvas, clientX, clientY);
    if (!point) return false;
    const previous = stroke.points[stroke.points.length - 1];
    stroke.points.push(point);
    const context = canvas.getContext("2d");
    if (previous && context) paintSegment(context, stroke, previous, point);
    return true;
  }

  function stopStroke(): boolean {
    if (!drawingRef.current) return false;
    drawingRef.current = null;
    return true;
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
      cancelStroke() {
        const stroke = drawingRef.current;
        if (!stroke) return;
        strokesRef.current = strokesRef.current.filter((it) => it !== stroke);
        drawingRef.current = null;
        repaint();
      },
      beginAt(clientX, clientY) {
        startStroke(clientX, clientY);
      },
      extendTo(clientX, clientY) {
        continueStroke(clientX, clientY);
      },
      endStroke() {
        stopStroke();
      },
      canvas: () => canvasRef.current,
    }),
    [],
  );

  function begin(event: ReactPointerEvent<HTMLCanvasElement>): void {
    const canvas = canvasRef.current;
    if (!event.isPrimary || !canvas) return;
    // An owner may have started this very stroke in the capture phase; then the
    // sheet has nothing to add and must not steal the pointer capture either.
    if (!startStroke(event.clientX, event.clientY)) return;
    event.preventDefault();
    canvas.setPointerCapture(event.pointerId);
  }

  function extend(event: ReactPointerEvent<HTMLCanvasElement>): void {
    // A second finger is a pinch, not the pen: never swallow its move, or the
    // container never sees the two points it needs to compute the pinch.
    if (!event.isPrimary) return;
    if (continueStroke(event.clientX, event.clientY)) event.preventDefault();
  }

  function finish(event: ReactPointerEvent<HTMLCanvasElement>): void {
    // A second finger lifting is a pinch ending, not the pen: never swallow it.
    if (!event.isPrimary) return;
    if (stopStroke()) event.preventDefault();
  }

  return (
    <canvas
      ref={canvasRef}
      aria-label={label}
      data-annotation={active ? "active" : "idle"}
      className={`[touch-action:none] ${active ? "pointer-events-auto cursor-crosshair" : "pointer-events-none"} ${className}`}
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
  className = "",
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
              color === pen.token ? "border-accent" : "border-edge-strong"
            }`}
          />
        </button>
      ))}
      <Button variant="secondary" onClick={onUndo} disabled={!strokeCount}>
        Undo
      </Button>
      <Button variant="secondary" onClick={onClear} disabled={!strokeCount}>
        Clear
      </Button>
    </div>
  );
}
