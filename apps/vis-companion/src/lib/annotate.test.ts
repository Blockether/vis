import { describe, expect, it } from 'vitest';
import {
  canvasPoint,
  paintStroke,
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
