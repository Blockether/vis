/**
 * The pure part of a live `canvas`: where each card and arrow stands, and where the
 * camera looks. Nothing here touches the DOM. `LiveCanvas` draws it and owns the
 * gestures.
 */

import viewSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/view.json';
import type { LiveCanvasCard, LiveCanvasEdge } from './live-view';
import { clamp, type Point, type Size } from './zoom-pan';

interface CanvasDefs {
  component: { oneOf: { properties: Record<string, { const?: unknown; maximum?: number }> }[] };
}

const CANVAS_BRANCH = (viewSchema.$defs as unknown as CanvasDefs).component.oneOf.find(
  (branch) => branch.properties.type?.const === 'canvas',
);
if (CANVAS_BRANCH?.properties.zoom?.maximum === undefined) {
  throw new Error('view.json has no canvas component');
}

/** The largest scale of a board (`view.json` canvas `zoom`). 1 is a card at full size. */
export const CANVAS_MAX_ZOOM = CANVAS_BRANCH.properties.zoom.maximum;
/** The smallest scale: a board of the largest size still fits in a phone. */
export const CANVAS_MIN_ZOOM = 0.1;
/** Below this scale the text of a card is too small to read: a card shows its title only. */
export const CANVAS_FAR_ZOOM = 0.55;

export const CANVAS_CARD_WIDTH = 260;
/** The room between two steps of the flow: an arrow and its label stand in it. */
export const CANVAS_STEP_GAP = 76;
/** The room between two cards of one step. */
export const CANVAS_CARD_GAP = 20;
/** The free band between the board and the frame when the board fits. */
export const CANVAS_PAD = 24;
/** A frame under this width has no room for two steps side by side. */
export const CANVAS_NARROW = 560;

/**
 * The direction of the flow. `right` puts the steps from left to right. `down` puts
 * them from top to bottom, so a board in a phone keeps a size that a person can read.
 */
export type CanvasFlow = 'right' | 'down';

/** The flow for a frame of this width. A frame without a size is a wide one. */
export function canvasFlow(width: number): CanvasFlow {
  return width > 0 && width < CANVAS_NARROW ? 'down' : 'right';
}
/** The lowest frame: a card at full size has room in it. */
export const CANVAS_MIN_HEIGHT = 200;

/** How far the ports of one card side stand apart. */
const PORT_STEP = 12;
const HEAD_LENGTH = 7;
const HEAD_HALF = 3.5;

export interface CanvasBox {
  id: string;
  x: number;
  y: number;
  width: number;
  height: number;
}

export interface CanvasArrow {
  /** `<source>><target>`: one arrow for each pair of cards. */
  id: string;
  from: string;
  to: string;
  /** The curve, in board pixels. */
  path: string;
  /** The points of the arrow head. */
  head: string;
  /** The middle of the curve: where the label stands. */
  middle: Point;
  edge: LiveCanvasEdge;
}

export interface CanvasLayout {
  boxes: CanvasBox[];
  arrows: CanvasArrow[];
  width: number;
  height: number;
}

/** The pan and the scale of a board. The origin is the top left corner of the frame. */
export interface CanvasCamera {
  x: number;
  y: number;
  scale: number;
}

/**
 * The height of a card before the surface measured it: the title line, three lines
 * of text at most and one line of badges.
 */
export function estimatedCardHeight(card: LiveCanvasCard): number {
  const textLines = card.text ? Math.min(3, Math.ceil(card.text.length / 34)) : 0;
  return 18 + 18 + textLines * 16 + (textLines ? 4 : 0) + (card.badges.length ? 24 : 0);
}

/**
 * The column of each card, from 0 with no gaps. A card with a `column` stands there.
 * Each other card stands one column after the last card that it comes from, so a
 * chain reads from left to right. A cycle is cut at the card where it closes.
 */
export function canvasColumns(cards: readonly LiveCanvasCard[]): Map<string, number> {
  const byId = new Map(cards.map((card) => [card.id, card]));
  const found = new Map<string, number>();
  const open = new Set<string>();
  const columnOf = (card: LiveCanvasCard): number => {
    const known = found.get(card.id);
    if (known !== undefined) return known;
    if (card.column !== undefined) {
      found.set(card.id, card.column);
      return card.column;
    }
    open.add(card.id);
    let column = 0;
    for (const edge of card.edges) {
      const source = byId.get(edge.from);
      if (source && !open.has(source.id)) column = Math.max(column, columnOf(source) + 1);
    }
    open.delete(card.id);
    found.set(card.id, column);
    return column;
  };
  for (const card of cards) columnOf(card);
  const ranks = [...new Set(found.values())].sort((a, b) => a - b);
  return new Map([...found].map(([id, column]) => [id, ranks.indexOf(column)]));
}

/** Ports spread about the middle of a card side, inside the side. */
function portOffset(index: number, count: number, side: number): number {
  const step = Math.min(PORT_STEP, Math.max(0, side - 16) / count);
  return (index - (count - 1) / 2) * step;
}

/** A card on the two axes of the flow: `along` the flow and `across` it. */
interface Slot {
  id: string;
  along: number;
  across: number;
  length: number;
  breadth: number;
}

/**
 * The place of each card and arrow. The steps of the flow stand one after the other
 * and each step is centred against the widest one. `heights` holds the measured
 * height of the cards that the surface has drawn.
 */
export function canvasLayout(
  cards: readonly LiveCanvasCard[],
  heights: ReadonlyMap<string, number> = new Map(),
  flow: CanvasFlow = 'right',
): CanvasLayout {
  const isDown = flow === 'down';
  const columns = canvasColumns(cards);
  const steps: LiveCanvasCard[][] = [];
  for (const card of cards) (steps[columns.get(card.id) ?? 0] ??= []).push(card);
  const heightOf = (card: LiveCanvasCard) => heights.get(card.id) ?? estimatedCardHeight(card);
  const lengthOf = (card: LiveCanvasCard) => (isDown ? heightOf(card) : CANVAS_CARD_WIDTH);
  const breadthOf = (card: LiveCanvasCard) => (isDown ? CANVAS_CARD_WIDTH : heightOf(card));
  const stepBreadth = (step: LiveCanvasCard[]) =>
    step.reduce((sum, card) => sum + breadthOf(card), 0) + (step.length - 1) * CANVAS_CARD_GAP;
  const breadth = Math.max(0, ...steps.map(stepBreadth));
  const slots = new Map<string, Slot>();
  let along = 0;
  for (const step of steps) {
    const ordered = step
      .map((card, index) => ({ card, key: card.row ?? index, index }))
      .sort((a, b) => a.key - b.key || a.index - b.index);
    let across = Math.round((breadth - stepBreadth(step)) / 2);
    for (const { card } of ordered) {
      slots.set(card.id, {
        id: card.id,
        along,
        across,
        length: lengthOf(card),
        breadth: breadthOf(card),
      });
      across += breadthOf(card) + CANVAS_CARD_GAP;
    }
    along += Math.max(...step.map(lengthOf)) + CANVAS_STEP_GAP;
  }
  const length = Math.max(0, along - CANVAS_STEP_GAP);

  // One arrow for each pair of cards: the last edge of a pair wins, as an append does.
  const links = new Map<string, { from: Slot; to: Slot; edge: LiveCanvasEdge }>();
  for (const card of cards) {
    const to = slots.get(card.id);
    for (const edge of card.edges) {
      const from = slots.get(edge.from);
      if (to && from && from !== to) links.set(`${from.id}>${to.id}`, { from, to, edge });
    }
  }
  // The ports of a side stand in the order of the cards at the other end, so two
  // arrows of one card do not cross at the card.
  const leaving = new Map<string, string[]>();
  const arriving = new Map<string, string[]>();
  const middle = (slot: Slot) => slot.across + slot.breadth / 2;
  for (const [id, { from, to }] of links) {
    leaving.set(from.id, [...(leaving.get(from.id) ?? []), id]);
    arriving.set(to.id, [...(arriving.get(to.id) ?? []), id]);
  }
  for (const ids of leaving.values()) {
    ids.sort((a, b) => middle(links.get(a)!.to) - middle(links.get(b)!.to));
  }
  for (const ids of arriving.values()) {
    ids.sort((a, b) => middle(links.get(a)!.from) - middle(links.get(b)!.from));
  }

  /** A point of the flow as a point of the board. */
  const at = (onFlow: number, acrossFlow: number): Point =>
    isDown ? { x: acrossFlow, y: onFlow } : { x: onFlow, y: acrossFlow };
  const text = ({ x, y }: Point) => `${x},${y}`;
  const arrows: CanvasArrow[] = [];
  for (const [id, { from, to, edge }] of links) {
    const out = leaving.get(from.id)!;
    const into = arriving.get(to.id)!;
    const start = from.along + from.length;
    const startAcross = middle(from) + portOffset(out.indexOf(id), out.length, from.breadth);
    const end = to.along;
    const endAcross = middle(to) + portOffset(into.indexOf(id), into.length, to.breadth);
    // An arrow to an earlier step leaves forward and comes back in a loop.
    const reach = end > start ? (end - start) / 2 : CANVAS_STEP_GAP;
    const tip = end - HEAD_LENGTH;
    arrows.push({
      id,
      from: from.id,
      to: to.id,
      path: `M${text(at(start, startAcross))} C${text(at(start + reach, startAcross))} ${text(
        at(tip - reach, endAcross),
      )} ${text(at(tip, endAcross))}`,
      head: [at(end, endAcross), at(tip, endAcross - HEAD_HALF), at(tip, endAcross + HEAD_HALF)]
        .map(text)
        .join(' '),
      middle: at((start + tip) / 2, (startAcross + endAcross) / 2),
      edge,
    });
  }

  const size = at(length, breadth);
  return {
    boxes: cards.map((card) => {
      const slot = slots.get(card.id)!;
      const corner = at(slot.along, slot.across);
      const extent = at(slot.length, slot.breadth);
      return { id: card.id, x: corner.x, y: corner.y, width: extent.x, height: extent.y };
    }),
    arrows,
    width: size.x,
    height: size.y,
  };
}

/** The scale at which `content` fits in `frame` with the pad around it. Never above 1. */
function fitScale(content: Size, frame: Size): number {
  if (content.width <= 0 || content.height <= 0) return 1;
  const scale = Math.min(
    (frame.width - 2 * CANVAS_PAD) / content.width,
    (frame.height - 2 * CANVAS_PAD) / content.height,
  );
  return clamp(scale, CANVAS_MIN_ZOOM, 1);
}

/**
 * Where the owner points the camera. `focus` names the card in the centre: no id, or
 * an id that the board does not hold, shows the whole board. `zoom` is the scale, and
 * 0 fits the focus in the frame. A frame without a size shows the board as it is.
 */
export function canvasTarget(
  layout: CanvasLayout,
  frame: Size,
  zoom: number,
  focus: string,
): CanvasCamera {
  if (frame.width <= 0 || frame.height <= 0 || layout.boxes.length === 0) {
    return { x: 0, y: 0, scale: 1 };
  }
  const box = (focus && layout.boxes.find((one) => one.id === focus)) || {
    x: 0,
    y: 0,
    width: layout.width,
    height: layout.height,
  };
  const scale = zoom > 0 ? clamp(zoom, CANVAS_MIN_ZOOM, CANVAS_MAX_ZOOM) : fitScale(box, frame);
  return {
    x: Math.round((frame.width - box.width * scale) / 2 - box.x * scale),
    y: Math.round((frame.height - box.height * scale) / 2 - box.y * scale),
    scale,
  };
}

/** The camera scaled about `point` of the frame: what is under the point stays there. */
export function canvasZoomedAt(camera: CanvasCamera, factor: number, point: Point): CanvasCamera {
  const scale = clamp(camera.scale * factor, CANVAS_MIN_ZOOM, CANVAS_MAX_ZOOM);
  const applied = scale / camera.scale;
  return {
    x: point.x - (point.x - camera.x) * applied,
    y: point.y - (point.y - camera.y) * applied,
    scale,
  };
}

/**
 * The height of the frame for a board in a frame of this width: the board at the
 * scale that fits the width, between the lowest frame and `maxHeight`.
 */
export function canvasFrameHeight(layout: CanvasLayout, width: number, maxHeight: number): number {
  if (width <= 0 || layout.width <= 0) return CANVAS_MIN_HEIGHT;
  const scale = clamp((width - 2 * CANVAS_PAD) / layout.width, CANVAS_MIN_ZOOM, 1);
  return clamp(
    Math.ceil(layout.height * scale + 2 * CANVAS_PAD),
    CANVAS_MIN_HEIGHT,
    Math.max(CANVAS_MIN_HEIGHT, maxHeight),
  );
}
