import { describe, expect, it } from 'vitest';
import primitives from '../../../../packages/vis-contract/resources/vis-contract/fixtures/live-primitives.json';
import viewSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/view.json';
import {
  CANVAS_MAX_ZOOM,
  canvasFlow,
  canvasLayout,
  canvasTarget,
  canvasZoomedAt,
  type CanvasBox,
  type CanvasLayout,
} from './canvas';
import { CANVAS_ROUTES, CANVAS_SHAPES } from './canvas-vocabulary';
import { liveViewFromWire, type LiveCanvasCard, type LiveCanvasEdge } from './live-view';
import type { Point } from './zoom-pan';

const edge = (from: string, more: Partial<LiveCanvasEdge> = {}): LiveCanvasEdge => ({
  from, is_active: false, route: 'curve', line: 'solid', start_mark: 'none', end_mark: 'arrow',
  animation: 'flow', ...more,
});
const card = (id: string, after: string[] = [], more: Partial<LiveCanvasCard> = {}): LiveCanvasCard => ({
  id, title: id, badges: [], edges: after.map((from) => edge(from)), kind: 'card',
  shape: 'rounded', fill: 'soft', border: 'solid', size: 'medium', ...more,
});
const diamond = () => [
  card('plan'), card('build', ['plan']), card('docs', ['plan']), card('verify', ['build', 'docs']),
];
const box = (layout: CanvasLayout, id: string) => layout.boxes.find((one) => one.id === id)!;
const centreY = (b: CanvasBox) => b.y + b.height / 2;
const showcase = liveViewFromWire(primitives)?.nodes.find((node) => node.id === 'canvas');
if (showcase?.type !== 'canvas') throw new Error('The fixture has no canvas');

function samplePath(path: string): Point[] {
  const samples: Point[] = [];
  let start = { x: 0, y: 0 };
  for (const command of path.match(/[MLQC][^MLQC]+/g)!) {
    const n = command.slice(1).match(/-?\d+(?:\.\d+)?/g)!.map(Number);
    const points = Array.from({ length: n.length / 2 }, (_, i) => ({ x: n[i * 2], y: n[i * 2 + 1] }));
    if (command[0] !== 'M') for (let i = 0; i <= 128; i++) {
      const t = i / 128;
      let sample = [start, ...points];
      while (sample.length > 1) sample = sample.slice(1).map((point, j) => ({
        x: sample[j].x + (point.x - sample[j].x) * t,
        y: sample[j].y + (point.y - sample[j].y) * t,
      }));
      samples.push(sample[0]);
    }
    start = points.at(-1)!;
  }
  return samples;
}

function pointIsInside({ x, y }: Point, b: CanvasBox) {
  return b.x + 0.01 < x && x < b.x + b.width - 0.01 && b.y + 0.01 < y && y < b.y + b.height - 0.01;
}

function checkBoxes(layout: CanvasLayout) {
  const { boxes, width, height } = layout;
  const byId = new Map(boxes.map((one) => [one.id, one]));
  function hasAncestor(one: CanvasBox, id: string): boolean {
    return one.parent === undefined ? false : one.parent === id || hasAncestor(byId.get(one.parent)!, id);
  }
  for (const [i, a] of boxes.entries()) {
    for (const n of [a.x, a.y, a.width, a.height, a.caption]) {
      expect(Number.isInteger(n) && n >= 0, a.id).toBe(true);
    }
    expect(a.x + a.width).toBeLessThanOrEqual(width);
    expect(a.y + a.height + a.caption).toBeLessThanOrEqual(height);
    if (a.parent) {
      const parent = byId.get(a.parent)!;
      expect(boxes.indexOf(parent)).toBeLessThan(i);
      expect(a.depth).toBe(parent.depth + 1);
      expect(a.x).toBeGreaterThan(parent.x);
      expect(a.y).toBeGreaterThan(parent.y + parent.head!);
      expect(a.x + a.width).toBeLessThan(parent.x + parent.width);
      expect(a.y + a.height + a.caption).toBeLessThan(parent.y + parent.height);
    }
    for (const b of boxes.slice(i + 1)) {
      if (hasAncestor(a, b.id) || hasAncestor(b, a.id)) continue;
      expect(a.x + a.width <= b.x || b.x + b.width <= a.x ||
        a.y + a.height + a.caption <= b.y || b.y + b.height + b.caption <= a.y,
      `${a.id} overlaps ${b.id}`).toBe(true);
    }
  }
  for (const arrow of layout.arrows) {
    for (const { x, y } of [arrow.start, arrow.end, arrow.middle]) {
      expect(x >= 0 && y >= 0 && x <= width && y <= height, arrow.id).toBe(true);
    }
  }
}

describe('the layout of a canvas', () => {
  it('follows the surface unless the owner sets the flow', () => {
    expect(canvasFlow(1200)).toBe('right');
    expect(canvasFlow(390)).toBe('down');
    expect(canvasFlow(1200, 'down')).toBe('down');
    expect(canvasFlow(390, 'right')).toBe('right');
    const right = canvasLayout(diamond());
    const down = canvasLayout(diamond(), undefined, 'down');
    expect(box(right, 'verify').x).toBeGreaterThan(box(right, 'build').x);
    expect(box(right, 'build').x).toBeGreaterThan(box(right, 'plan').x);
    expect(box(down, 'verify').y).toBeGreaterThan(box(down, 'build').y);
    expect(box(down, 'build').y).toBeGreaterThan(box(down, 'plan').y);
  });

  it('orders steps without drawing hidden edges and aligns each branch with its source', () => {
    const layout = canvasLayout([
      card('top', [], { row: 0, column: 10 }),
      card('bottom', [], { row: 1, column: 10 }),
      card('second-bottom', [], { edges: [edge('bottom', { line: 'none' })] }),
      card('second-top', ['top']),
    ]);
    expect(layout.arrows.map((arrow) => arrow.id)).toEqual(['top>second-top']);
    expect(box(layout, 'top').x).toBe(box(layout, 'bottom').x);
    expect(box(layout, 'second-top').x).toBeGreaterThan(box(layout, 'top').x);
    expect(box(layout, 'second-top').x).toBe(box(layout, 'second-bottom').x);
    expect(centreY(box(layout, 'top'))).toBe(centreY(box(layout, 'second-top')));
    expect(centreY(box(layout, 'bottom'))).toBe(centreY(box(layout, 'second-bottom')));
  });

  it('uses the last arrow of a pair and skips absent sources and self links', () => {
    const cards = [card('plan'), card('build', [], {
      edges: [edge('plan'), edge('plan', { route: 'elbow' }), edge('gone'), edge('build')],
    })];
    const { arrows } = canvasLayout(cards);
    expect(arrows.map((arrow) => [arrow.id, arrow.edge.route])).toEqual([['plan>build', 'elbow']]);
  });

  it.each(['right', 'down'] as const)('holds the showcase and its captions in the %s flow', (flow) => {
    const measures = new Map(showcase.cards.map((one) => [one.id, { height: 160.5, caption: 31.25 }]));
    const layout = canvasLayout(showcase.cards, measures, flow);
    checkBoxes(layout);
    expect(new Set(layout.boxes.map((one) => one.id))).toEqual(new Set(showcase.cards.map((one) => one.id)));
    expect(box(layout, 'checks').parent).toBe('delivery');
    expect(box(layout, 'verify').depth).toBe(2);
    for (const one of showcase.cards.filter((item) => item.caption)) {
      expect(box(layout, one.id).caption).toBeGreaterThanOrEqual(31.25);
    }
    const axis = flow === 'right' ? 'x' : 'y';
    expect(box(layout, 'delivery')[axis]).toBeGreaterThan(box(layout, 'plan')[axis]);
    expect(box(layout, 'ready')[axis]).toBeGreaterThan(box(layout, 'delivery')[axis]);
    expect(canvasLayout(showcase.cards, measures, flow)).toEqual(layout);
  });

  it('cuts cycles and puts items with invalid parents or excessive depth on the board', () => {
    const layout = canvasLayout([
      card('missing', [], { parent: 'gone' }),
      card('not-a-frame', [], { parent: 'missing' }),
      card('cycle-a', ['cycle-b'], { kind: 'frame', parent: 'cycle-b' }),
      card('cycle-b', ['cycle-a'], { kind: 'frame', parent: 'cycle-a' }),
      ...Array.from({ length: 7 }, (_, i) => card(`f${i}`, [], { kind: 'frame', parent: i ? `f${i - 1}` : undefined })),
    ]);
    checkBoxes(layout);
    for (const id of ['missing', 'not-a-frame', 'cycle-a', 'cycle-b', 'f5']) {
      expect(box(layout, id).parent).toBeUndefined();
    }
    expect(box(layout, 'cycle-a').x).not.toBe(box(layout, 'cycle-b').x);
    expect(box(layout, 'f4').depth).toBe(4);
    expect(box(layout, 'f6').parent).toBe('f5');
  });

  it('lays out a full board with frames within one hundred milliseconds', () => {
    const full = Array.from({ length: 200 }, (_, i) => {
      const id = String(i);
      const group = i - i % 20;
      return card(id, i ? [String(i - 1)] : [], i % 20 ? { parent: String(group) } : { kind: 'frame' });
    });
    // The best of three runs: a busy machine can slow one run, not the layout.
    const times = [0, 1, 2].map(() => {
      const start = performance.now();
      expect(canvasLayout(full).boxes).toHaveLength(full.length);
      return performance.now() - start;
    });
    expect(Math.min(...times)).toBeLessThan(100);
  });
});

describe('the arrows of a canvas', () => {
  it.each(['right', 'down'] as const)('keeps arrows and labels out of other items in the %s flow', (flow) => {
    const measured = new Map(showcase.cards.map((item, i) => [item.id, { height: 68.5 + 13 * i, caption: 30 }]));
    for (const measure of [undefined, measured]) for (const route of [undefined, ...CANVAS_ROUTES]) {
      const cards = route ? showcase.cards.map((item) => ({ ...item, edges: item.edges.map((edge) => ({ ...edge, route })) })) : showcase.cards;
      const { arrows, boxes } = canvasLayout(cards, measure, flow);
      const labels = new Set<string>();
      for (const arrow of arrows) {
        const allowed = new Set<string>();
        for (const end of [arrow.from, arrow.to]) {
          for (let item = boxes.find((box) => box.id === end); item; item = boxes.find((box) => box.id === item?.parent)) {
            allowed.add(item.id);
          }
        }
        const other = boxes.filter((box) => !allowed.has(box.id));
        const samples = samplePath(arrow.path);
        const blocked = other.find((box) => samples.some((point) => pointIsInside(point, box)));
        expect(blocked?.id, `${arrow.id} (${route ?? 'fixture routes'})`).toBeUndefined();
        if (!arrow.edge.label) continue;
        const { x, y } = arrow.middle;
        const body = boxes.find((box) => box.head === undefined && pointIsInside(arrow.middle, box));
        expect(body?.id, arrow.id).toBeUndefined();
        const nearest = Math.min(...samples.map((p) => Math.hypot(p.x - x, p.y - y)));
        expect(nearest, arrow.id).toBeLessThan(4);
        const key = `${x},${y}`;
        expect(labels.has(key), arrow.id).toBe(false);
        labels.add(key);
      }
    }
  });

  function detourBoard() {
    return canvasLayout([
      card('a', [], { column: 0, row: 0, size: 'small' }),
      card('wide', [], { column: 0, row: 1, size: 'large' }),
      card('blocker', [], { column: 1, kind: 'frame' }),
      card('b', [], { column: 2, size: 'large', edges: [edge('a', { route: 'straight', label: 'Detour' }), edge('wide', { line: 'none' })] }),
    ]);
  }

  it('keeps a detour at least fourteen pixels away from a frame', () => {
    const layout = detourBoard();
    const blocker = box(layout, 'blocker');
    const clearance = (p: Point) => Math.hypot(
      Math.max(0, blocker.x - p.x, p.x - blocker.x - blocker.width),
      Math.max(0, blocker.y - p.y, p.y - blocker.y - blocker.height),
    );
    expect(Math.min(...samplePath(layout.arrows[0].path).map(clearance))).toBeGreaterThanOrEqual(13.999);
  });

  it('runs at least eighteen pixels from each port before turning', () => {
    const { start, end, path } = detourBoard().arrows[0];
    const commands = path.match(/[MLQC][^MLQC]+/g)!;
    const point = (command: string) => command.match(/-?\d+(?:\.\d+)?/g)!.slice(-2).map(Number);
    const [firstX, firstY] = point(commands[1]);
    const [lastX, lastY] = point(commands.at(-2)!);
    expect(firstY).toBe(start.y);
    expect(lastY).toBe(end.y);
    expect(firstX - start.x).toBeGreaterThanOrEqual(18);
    expect(end.x - lastX).toBeGreaterThanOrEqual(18);
  });

  it.each(['diamond', 'ellipse', 'hexagon', 'pill'] as const)('spreads incoming ports on the %s at least twelve pixels apart', (shape) => {
    const sources = Array.from({ length: 6 }, (_, i) => card(`item-${i}`, [], { column: 0 }));
    const { arrows } = canvasLayout([...sources, card('target', sources.map((item) => item.id), {
      column: 1, kind: 'shape', shape, size: 'small',
    })], new Map([['target', { height: 72 }]]));
    const tips = arrows.map((arrow) => arrow.end).sort((a, b) => a.y - b.y);
    for (const [i, point] of tips.slice(1).entries()) {
      expect(Math.hypot(point.x - tips[i].x, point.y - tips[i].y)).toBeGreaterThanOrEqual(11.999);
    }
  });

  it('centres the label on the longest straight run of a detour', () => {
    const { middle, path } = detourBoard().arrows[0];
    const commands = path.match(/[MLQC][^MLQC]+/g)!;
    const point = (command: string) => command.match(/-?\d+(?:\.\d+)?/g)!.slice(-2).map(Number);
    const runs = commands.slice(1).flatMap((command, i) => {
      if (command[0] !== 'L') return [];
      const [x1, y1] = point(commands[i]);
      const [x2, y2] = point(command);
      return [{ length: Math.hypot(x1 - x2, y1 - y2), x: (x1 + x2) / 2, y: (y1 + y2) / 2 }];
    }).sort((a, b) => b.length - a.length);
    expect(middle.x).toBeCloseTo(runs[0].x, 2);
    expect(middle.y).toBeCloseTo(runs[0].y, 2);
  });

  it('uses different runs for two detours through the same gap', () => {
    const { arrows, boxes } = canvasLayout([
      card('start', [], { column: 0 }),
      card('blocker', [], { column: 1 }),
      card('first', [], { column: 2, edges: [edge('start', { route: 'straight', label: 'First' })] }),
      card('second', [], { column: 2, edges: [edge('start', { route: 'straight', label: 'Second' })] }),
    ]);
    const blocker = boxes.find((b) => b.id === 'blocker')!;
    const halfway = blocker.x + blocker.width / 2;
    function heights(path: string): number[] {
      const points = samplePath(path);
      return points.slice(1).flatMap((point, i) => {
        const prev = points[i];
        if (!(Math.min(point.x, prev.x) < halfway && halfway <= Math.max(point.x, prev.x))) return [];
        const fraction = (halfway - prev.x) / (point.x - prev.x);
        return [prev.y + (point.y - prev.y) * fraction];
      });
    }
    const [first, second] = arrows.map((arrow) => heights(arrow.path));
    expect(first).toHaveLength(1);
    expect(second).toHaveLength(1);
    expect(Math.abs(first[0] - second[0])).toBeGreaterThanOrEqual(10);
    expect(arrows[0].start.y).not.toBe(arrows[1].start.y);
  });

  it('draws a straight line and centres its label on it', () => {
    const layout = canvasLayout([card('a'), card('b', [], { edges: [edge('a', { route: 'straight', end_mark: 'triangle' })] })]);
    const { path, middle, end } = layout.arrows[0];
    expect(path.match(/[MLCQ]/g)).toEqual(['M', 'L']);
    const [x1, y1, x2, y2] = path.match(/-?\d+(?:\.\d+)?/g)!.map(Number);
    expect(x2).toBeLessThan(end.x);
    expect(middle.x).toBeCloseTo((x1 + x2) / 2, 2);
    expect(middle.y).toBeCloseTo((y1 + y2) / 2, 2);
  });

  it('draws an elbow as horizontal and vertical runs with square turns', () => {
    const layout = canvasLayout(diamond().map((one) => ({
      ...one, edges: one.edges.map((link) => ({ ...link, route: 'elbow' })),
    })));
    const { path } = layout.arrows[0];
    const points = path.match(/[A-Z][^A-Z]+/g)!.map((part) =>
      [part[0], ...part.slice(1).match(/-?\d+(?:\.\d+)?/g)!.map(Number)] as [string, number, number]);
    expect(points.map(([command]) => command).join('')).toMatch(/^ML{3,}$/);
    for (let i = 1; i < points.length; i++) {
      expect(points[i][1] === points[i - 1][1] || points[i][2] === points[i - 1][2]).toBe(true);
    }
  });

  it.each(CANVAS_SHAPES)('puts spread ports on the %s outline in both flows', (shape) => {
    for (const flow of ['right', 'down'] as const) {
      const peers = Array.from({ length: 7 }, (_, i) => card(`${i}`, ['shape'], { column: 1 }));
      const layout = canvasLayout([
        card('shape', peers.map((one) => one.id), { kind: 'shape', shape, column: 0 }),
        ...peers,
      ], undefined, flow);
      const b = box(layout, 'shape');
      expect(layout.arrows).toHaveLength(2 * peers.length);
      const ports = layout.arrows.map((arrow) => arrow.from === 'shape' ? arrow.start : arrow.end);
      expect(new Set(ports.map((port) => flow === 'right' ? port.y : port.x)).size).toBe(ports.length);
      for (const port of ports) {
        const x = (port.x - b.x) / b.width;
        const y = (port.y - b.y) / b.height;
        let expected = 1;
        if (shape === 'ellipse') expected = 0.5 + Math.sqrt(0.25 - ((flow === 'right' ? y : x) - 0.5) ** 2);
        if (shape === 'diamond') expected = 1 - Math.abs((flow === 'right' ? y : x) - 0.5);
        if (shape === 'hexagon') expected = flow === 'right' ? 1 - 0.4 * Math.abs(y - 0.5) : 1 - Math.max(0, x - 0.8, 0.2 - x) * 2.5;
        if (shape === 'parallelogram') expected = flow === 'right' ? 1 - 0.2 * y : 1 - Math.max(0, x - 0.8) * 5;
        if (shape === 'cylinder') {
          if (flow === 'down') expected = 0.84 + 0.16 * Math.sqrt(1 - (2 * x - 1) ** 2);
          else {
            const cap = Math.max(0, 0.16 - y, y - 0.84) / 0.16;
            expected = 0.5 + 0.5 * Math.sqrt(1 - cap ** 2);
          }
        }
        if (shape === 'document' && flow === 'right' && y > 0.88) {
          expected = 0.5 * Math.sqrt((1 - y) / 0.12);
        }
        if (shape === 'document' && flow === 'down') {
          const t = 2 * x - 1;
          expected = t < 0 ? 1 - 0.12 * (t + 1) ** 2 : 0.88 - 0.24 * t * (1 - t);
        }
        if (shape === 'rounded' || shape === 'pill') {
          const r = Math.min(shape === 'pill' ? b.height / 2 : 12, b.width / 2);
          const cross = flow === 'right' ? b.height : b.width;
          const main = flow === 'right' ? b.width : b.height;
          const delta = Math.max(0, Math.abs((flow === 'right' ? y : x) - 0.5) * cross - (cross / 2 - r));
          expected = (main - r + Math.sqrt(r * r - delta * delta)) / main;
        }
        const actual = flow === 'right' ? x : y;
        expect(Math.abs(expected - actual) * (flow === 'right' ? b.width : b.height)).toBeLessThan(0.5);
      }
    }
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

  // Report: a board of the largest size of the contract did not lay out in minutes.
  it('lays out the largest board of the contract, with an arrow for each edge', () => {
    const branches = viewSchema.$defs.component.oneOf as unknown as {
      properties: { type?: { const?: string }; cards?: { maxItems: number } };
    }[];
    const most = branches.find((branch) => branch.properties.type?.const === 'canvas')!.properties.cards!.maxItems;
    const arrows = viewSchema.$defs.canvas_card.properties.edges.maxItems;
    const cards = Array.from({ length: most }, (_, i) =>
      card(`n${i}`, Array.from({ length: Math.min(arrows, i) }, (_, k) => `n${i - 1 - k}`)));
    const layout = canvasLayout(cards);
    expect(layout.boxes).toHaveLength(most);
    expect(layout.arrows).toHaveLength(cards.reduce((n, one) => n + one.edges.length, 0));
  }, 5000);
});
