import { describe, expect, it } from 'vitest';
import {
  PEN_COLORS,
  canvasPoint,
  paintSegment,
  paintStroke,
  repaintStrokes,
  strokeWidthFor,
  type Stroke,
} from './annotate';

/** A context that records what a pen asked for instead of painting it. */
function recordingContext() {
  const calls: string[] = [];
  const context = {
    strokeStyle: '',
    fillStyle: '',
    lineWidth: 0,
    lineCap: '',
    lineJoin: '',
    beginPath: () => calls.push('beginPath'),
    moveTo: (x: number, y: number) => calls.push(`moveTo(${x},${y})`),
    lineTo: (x: number, y: number) => calls.push(`lineTo(${x},${y})`),
    arc: (x: number, y: number, r: number) => calls.push(`arc(${x},${y},${r})`),
    fill: () => calls.push('fill'),
    stroke: () => calls.push('stroke'),
    clearRect: (x: number, y: number, w: number, h: number) =>
      calls.push(`clearRect(${x},${y},${w},${h})`),
  };
  return { calls, context: context as unknown as CanvasRenderingContext2D };
}

function stroke(points: Stroke['points']): Stroke {
  return { color: '#ff0000', width: 6, points };
}

describe('pen ink', () => {
  // The ink is theme ink: the gateway repaints the whole app from `/v1/theme`,
  // so a stroke frozen as hex is the one mark on the picture from another
  // product.
  it('names its colours by palette variable, never as hex', () => {
    expect(PEN_COLORS).toHaveLength(5);
    for (const pen of PEN_COLORS) {
      expect(pen.token.startsWith('--')).toBe(true);
      expect(pen.label).toMatch(/pen$/u);
    }
  });

  // A nib in picture pixels means the same gesture reads the same on a phone
  // screenshot and on a rendered A4 page instead of landing as a hairline.
  it('scales the nib to the picture, with a floor', () => {
    expect(strokeWidthFor(1000, 500)).toBe(6);
    expect(strokeWidthFor(100, 80)).toBe(3);
  });
});

describe('painting strokes', () => {
  // A tap is a legitimate annotation — circling one number starts as one point,
  // and a polyline of a single point paints absolutely nothing.
  it('paints a single tap as a dot', () => {
    const { calls, context } = recordingContext();
    paintStroke(context, stroke([{ x: 10, y: 20 }]));
    expect(calls).toEqual(['beginPath', 'arc(10,20,3)', 'fill']);
  });

  it('paints a drag as a polyline', () => {
    const { calls, context } = recordingContext();
    paintStroke(
      context,
      stroke([
        { x: 0, y: 0 },
        { x: 5, y: 5 },
        { x: 9, y: 1 },
      ]),
    );
    expect(calls).toEqual([
      'beginPath',
      'moveTo(0,0)',
      'lineTo(5,5)',
      'lineTo(9,1)',
      'stroke',
    ]);
  });

  it('ignores a stroke with no points at all', () => {
    const { calls, context } = recordingContext();
    paintStroke(context, stroke([]));
    expect(calls).toEqual([]);
  });

  // Undo and Clear are exactly this: an empty layer, then whatever survived.
  it('repaints every surviving stroke from an empty layer', () => {
    const { calls, context } = recordingContext();
    const canvas = { width: 40, height: 30 } as HTMLCanvasElement;
    repaintStrokes(context, canvas, [stroke([{ x: 1, y: 2 }])]);
    expect(calls[0]).toBe('clearRect(0,0,40,30)');
    expect(calls).toContain('arc(1,2,3)');
  });

  // Repainting the whole layer on every pointer move is what makes a pen feel
  // like it is dragging behind the finger.
  it('paints only the segment the hand just moved through', () => {
    const { calls, context } = recordingContext();
    paintSegment(context, stroke([]), { x: 1, y: 1 }, { x: 4, y: 8 });
    expect(calls).toEqual([
      'beginPath',
      'moveTo(1,1)',
      'lineTo(4,8)',
      'stroke',
    ]);
  });
});

describe('where a pointer landed', () => {
  const canvas = {
    width: 1000,
    height: 500,
    getBoundingClientRect: () => ({
      left: 100,
      top: 50,
      width: 500,
      height: 250,
    }),
  } as unknown as HTMLCanvasElement;

  // The canvas is displayed at whatever size the layout gave it, so a stroke
  // recorded in screen pixels drifts the moment the window changes.
  it('reads the picture own pixels, not the screen', () => {
    expect(canvasPoint(canvas, 350, 175)).toEqual({ x: 500, y: 250 });
  });

  it('refuses a canvas that has not been laid out', () => {
    const hidden = {
      width: 10,
      height: 10,
      getBoundingClientRect: () => ({ left: 0, top: 0, width: 0, height: 0 }),
    } as unknown as HTMLCanvasElement;
    expect(canvasPoint(hidden, 5, 5)).toBeNull();
  });
});
