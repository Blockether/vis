import { describe, expect, it } from 'vitest';
import {
  CANVAS_MAX_ZOOM,
  canvasColumns,
  canvasFlow,
  canvasLayout,
  canvasTarget,
  canvasZoomedAt,
} from './canvas';
import type { LiveCanvasCard } from './live-view';

const card = (id: string, after: string[] = [], more: Partial<LiveCanvasCard> = {}): LiveCanvasCard => ({
  id,
  title: id,
  badges: [],
  edges: after.map((from) => ({ from, is_active: false })),
  ...more,
});
const diamond = () => [
  card('plan'),
  card('build', ['plan']),
  card('docs', ['plan']),
  card('verify', ['build', 'docs']),
];
const box = (cards: LiveCanvasCard[], id: string, flow: 'right' | 'down' = 'right') =>
  canvasLayout(cards, new Map(), flow).boxes.find((one) => one.id === id)!;

describe('the steps of a canvas', () => {
  it('puts a card one step after the last card that it comes from', () => {
    expect([...canvasColumns(diamond())]).toEqual([
      ['plan', 0],
      ['build', 1],
      ['docs', 1],
      ['verify', 2],
    ]);
  });

  it('keeps an explicit column and leaves no empty step', () => {
    const columns = canvasColumns([card('a', [], { column: 4 }), card('b', ['a']), card('c')]);
    expect([columns.get('c'), columns.get('a'), columns.get('b')]).toEqual([0, 1, 2]);
  });

  it('places every card of a cycle', () => {
    const columns = canvasColumns([card('a', ['c']), card('b', ['a']), card('c', ['b'])]);
    expect(new Set(columns.values())).toEqual(new Set([0, 1, 2]));
  });
});

describe('the layout of a canvas', () => {
  it('runs the steps from left to right on a wide surface and from top to bottom on a narrow one', () => {
    expect(canvasFlow(1200)).toBe('right');
    expect(canvasFlow(390)).toBe('down');
    expect(box(diamond(), 'verify').x).toBeGreaterThan(box(diamond(), 'build').x);
    expect(box(diamond(), 'build').x).toBeGreaterThan(box(diamond(), 'plan').x);
    expect(box(diamond(), 'verify', 'down').y).toBeGreaterThan(box(diamond(), 'build', 'down').y);
    expect(box(diamond(), 'build', 'down').y).toBeGreaterThan(box(diamond(), 'plan', 'down').y);
  });

  it('orders the cards of one step by row and keeps them apart', () => {
    const cards = [card('plan'), card('build', ['plan'], { row: 1 }), card('docs', ['plan'], { row: 0 })];
    const [build, docs] = [box(cards, 'build'), box(cards, 'docs')];
    expect(build.x).toBe(docs.x);
    expect(build.y).toBeGreaterThanOrEqual(docs.y + docs.height);
  });

  it('draws one arrow for each pair of cards and none from a card that the board does not hold', () => {
    const cards = [card('plan'), card('build', ['plan', 'plan', 'gone', 'build'])];
    expect(canvasLayout(cards).arrows.map((arrow) => arrow.id)).toEqual(['plan>build']);
  });
});

describe('the camera of a canvas', () => {
  const layout = canvasLayout(diamond());
  const frame = { width: 400, height: 300 };
  const centre = (id: string, camera: { x: number; y: number; scale: number }) => {
    const one = layout.boxes.find((it) => it.id === id)!;
    return {
      x: camera.x + (one.x + one.width / 2) * camera.scale,
      y: camera.y + (one.y + one.height / 2) * camera.scale,
    };
  };

  it('fits the whole board in the frame and never enlarges a small board', () => {
    const camera = canvasTarget(layout, frame, 0, '');
    expect(camera.scale).toBeLessThan(1);
    expect(camera.x).toBeGreaterThanOrEqual(0);
    expect(camera.x + layout.width * camera.scale).toBeLessThanOrEqual(frame.width);
    expect(camera.y + layout.height * camera.scale).toBeLessThanOrEqual(frame.height);
    expect(canvasTarget(canvasLayout([card('one')]), { width: 2000, height: 2000 }, 0, '').scale).toBe(1);
  });

  it('puts the focus card in the centre and shows the whole board for an unknown focus', () => {
    const at = centre('verify', canvasTarget(layout, frame, 1, 'verify'));
    expect(Math.abs(at.x - frame.width / 2)).toBeLessThanOrEqual(1);
    expect(Math.abs(at.y - frame.height / 2)).toBeLessThanOrEqual(1);
    expect(canvasTarget(layout, frame, 0, 'gone')).toEqual(canvasTarget(layout, frame, 0, ''));
  });

  it('keeps the point under the pointer and stops at the largest zoom', () => {
    const camera = { x: 30, y: -20, scale: 0.5 };
    const point = { x: 120, y: 80 };
    const under = (one: typeof camera) => ({
      x: (point.x - one.x) / one.scale,
      y: (point.y - one.y) / one.scale,
    });
    const zoomed = canvasZoomedAt(camera, 2, point);
    expect(zoomed.scale).toBe(1);
    expect(under(zoomed).x).toBeCloseTo(under(camera).x);
    expect(under(zoomed).y).toBeCloseTo(under(camera).y);
    expect(canvasZoomedAt(camera, 1000, point).scale).toBe(CANVAS_MAX_ZOOM);
  });
});
