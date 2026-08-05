import { canvasPngBlob } from './image-file';
import type { Point } from './zoom-pan';

/**
 * Drawing ON something, as a model: what a stroke is, what colour it can be,
 * and how strokes are painted and flattened into the picture they annotate.
 *
 * This is the half of annotation that has nothing to do with a picture viewer,
 * which is why it is not inside one. A PDF page, an HTML artifact and a plain
 * attachment all annotate through exactly these functions, and a test can put a
 * stroke through them without a browser, a pointer or a screen.
 */

export type Stroke = { color: string; width: number; points: Point[] };

/**
 * Pen colours are THEME colours, named by palette variable rather than frozen
 * as hex: `/v1/theme` repaints the whole app from the gateway, and an ink that
 * ignores it is the one mark on the picture that belongs to another product.
 * The canvas needs a real colour string, so the value is read from the live
 * palette at the moment a stroke starts.
 */
export const PEN_COLORS = [
  { token: '--err', className: 'bg-err', label: 'Red pen' },
  { token: '--fg', className: 'bg-white', label: 'Ink pen' },
  { token: '--primary', className: 'bg-accent', label: 'Amber pen' },
  { token: '--link-fg', className: 'bg-link', label: 'Blue pen' },
  { token: '--ok', className: 'bg-ok', label: 'Green pen' },
] as const;

export type PenToken = (typeof PEN_COLORS)[number]['token'];

/** The live value behind a palette variable — canvas work cannot use a class. */
export function paletteColor(token: string): string {
  const value = getComputedStyle(document.documentElement)
    .getPropertyValue(token)
    .trim();
  return value || '#ef4444';
}

/**
 * How wide the nib is on THIS picture. A stroke is a fraction of the picture's
 * own pixels, so the same gesture reads the same on a phone screenshot and on a
 * rendered A4 page, and never lands as a hairline on the big one.
 */
export function strokeWidthFor(width: number, height: number): number {
  return Math.max(3, Math.max(width, height) * 0.006);
}

/**
 * Where a pointer landed in the picture's OWN pixels. The canvas is displayed
 * at whatever size the layout gave it, so a stroke has to be recorded in the
 * grid it will be flattened into, or an edit drifts when the window changes.
 */
export function canvasPoint(
  canvas: HTMLCanvasElement,
  clientX: number,
  clientY: number,
): Point | null {
  const rect = canvas.getBoundingClientRect();
  if (!rect.width || !rect.height) return null;
  return {
    x: ((clientX - rect.left) / rect.width) * canvas.width,
    y: ((clientY - rect.top) / rect.height) * canvas.height,
  };
}

function nib(context: CanvasRenderingContext2D, stroke: Stroke): void {
  context.strokeStyle = stroke.color;
  context.fillStyle = stroke.color;
  context.lineWidth = stroke.width;
  context.lineCap = 'round';
  context.lineJoin = 'round';
}

/** One stroke. A single tap is a DOT — a polyline of one point paints nothing. */
export function paintStroke(
  context: CanvasRenderingContext2D,
  stroke: Stroke,
): void {
  const first = stroke.points[0];
  if (!first) return;
  nib(context, stroke);
  if (stroke.points.length === 1) {
    context.beginPath();
    context.arc(first.x, first.y, stroke.width / 2, 0, Math.PI * 2);
    context.fill();
    return;
  }
  context.beginPath();
  context.moveTo(first.x, first.y);
  for (const point of stroke.points.slice(1)) context.lineTo(point.x, point.y);
  context.stroke();
}

/** Every stroke from an empty layer: undo and clear are exactly this. */
export function repaintStrokes(
  context: CanvasRenderingContext2D,
  canvas: HTMLCanvasElement,
  strokes: readonly Stroke[],
): void {
  context.clearRect(0, 0, canvas.width, canvas.height);
  for (const stroke of strokes) paintStroke(context, stroke);
}

/**
 * Just the segment the hand moved through. Repainting the whole layer on every
 * pointer move is what makes a pen feel like it is dragging behind the finger.
 */
export function paintSegment(
  context: CanvasRenderingContext2D,
  stroke: Stroke,
  from: Point,
  to: Point,
): void {
  nib(context, stroke);
  context.beginPath();
  context.moveTo(from.x, from.y);
  context.lineTo(to.x, to.y);
  context.stroke();
}

/**
 * The picture with its annotations burned in, at the picture's own resolution.
 * The original bytes are never touched: this is a NEW image, which is what lets
 * a drawn-on document page travel to the model while the document stays put.
 */
export function flattenAnnotations(
  picture: HTMLImageElement,
  annotations: HTMLCanvasElement | null,
): Promise<Blob> {
  const width = picture.naturalWidth;
  const height = picture.naturalHeight;
  if (!width || !height) throw new Error('Image is not ready');
  const output = document.createElement('canvas');
  output.width = width;
  output.height = height;
  const context = output.getContext('2d');
  if (!context) throw new Error('Image editing is unavailable');
  context.drawImage(picture, 0, 0, width, height);
  if (annotations?.width && annotations.height) {
    context.drawImage(annotations, 0, 0, width, height);
  }
  return canvasPngBlob(output);
}
