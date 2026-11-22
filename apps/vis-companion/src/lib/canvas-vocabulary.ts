/**
 * The closed vocabulary of a live `canvas`, read from `view.json`. The types name the
 * words for the compiler. The lists and the defaults come from the schema.
 */

import viewSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/view.json';

export type CanvasKind = 'card' | 'shape' | 'note' | 'comment' | 'frame' | 'text';
export type CanvasShape =
  | 'rectangle'
  | 'rounded'
  | 'pill'
  | 'ellipse'
  | 'diamond'
  | 'hexagon'
  | 'parallelogram'
  | 'cylinder'
  | 'document';
export type CanvasColor =
  | 'gray'
  | 'red'
  | 'orange'
  | 'yellow'
  | 'green'
  | 'teal'
  | 'blue'
  | 'purple'
  | 'pink';
export type CanvasFill = 'none' | 'soft' | 'solid';
export type CanvasLine = 'solid' | 'dashed' | 'dotted' | 'double' | 'thick' | 'none';
export type CanvasSize = 'small' | 'medium' | 'large';
export type CanvasAnimation = 'pulse' | 'glow' | 'bounce' | 'shake';
export type CanvasRoute = 'curve' | 'straight' | 'elbow';
export type CanvasMark = 'none' | 'arrow' | 'triangle' | 'circle' | 'diamond' | 'bar';
export type CanvasEdgeAnimation = 'flow' | 'dash' | 'dot' | 'pulse';
/** `auto` lets the surface choose from its width. */
export type CanvasFlowSetting = 'auto' | 'right' | 'down';
export type CanvasPattern = 'dots' | 'grid' | 'lines' | 'plain';
export type CanvasPictureFit = 'cover' | 'contain';
export type CanvasMediaKind = 'video' | 'audio';

const DEFS = viewSchema.$defs;

function words<T extends string>(def: { enum: string[] }): readonly T[] {
  return def.enum as unknown as readonly T[];
}

export const CANVAS_KINDS = words<CanvasKind>(DEFS.canvas_kind);
export const CANVAS_SHAPES = words<CanvasShape>(DEFS.canvas_shape);
export const CANVAS_COLORS = words<CanvasColor>(DEFS.canvas_color);
export const CANVAS_FILLS = words<CanvasFill>(DEFS.canvas_fill);
export const CANVAS_LINES = words<CanvasLine>(DEFS.canvas_line);
export const CANVAS_SIZES = words<CanvasSize>(DEFS.canvas_size);
export const CANVAS_ANIMATIONS = words<CanvasAnimation>(DEFS.canvas_animation);
export const CANVAS_ROUTES = words<CanvasRoute>(DEFS.canvas_route);
export const CANVAS_MARKS = words<CanvasMark>(DEFS.canvas_mark);
export const CANVAS_EDGE_ANIMATIONS = words<CanvasEdgeAnimation>(DEFS.canvas_edge_animation);
export const CANVAS_FLOWS = words<CanvasFlowSetting>(DEFS.canvas_flow);
export const CANVAS_PATTERNS = words<CanvasPattern>(DEFS.canvas_pattern);
export const CANVAS_PICTURE_FITS = words<CanvasPictureFit>(DEFS.canvas_picture.properties.fit);
export const CANVAS_MEDIA_KINDS = words<CanvasMediaKind>(DEFS.canvas_media.properties.kind);

/** A reader of one word: an absent or unknown word is the schema default. */
function word<T extends string>(all: readonly T[], fallback: string): (raw: unknown) => T {
  return (raw) => ((all as readonly unknown[]).includes(raw) ? (raw as T) : (fallback as T));
}

/** A reader of one word that has no default: an absent or unknown word is nothing. */
function optionalWord<T extends string>(all: readonly T[]): (raw: unknown) => T | undefined {
  return (raw) => ((all as readonly unknown[]).includes(raw) ? (raw as T) : undefined);
}

export const canvasKind = word(CANVAS_KINDS, DEFS.canvas_kind.default);
export const canvasShape = word(CANVAS_SHAPES, DEFS.canvas_shape.default);
export const canvasColor = optionalWord(CANVAS_COLORS);
export const canvasFill = word(CANVAS_FILLS, DEFS.canvas_fill.default);
export const canvasLine = word(CANVAS_LINES, DEFS.canvas_line.default);
export const canvasSize = word(CANVAS_SIZES, DEFS.canvas_size.default);
export const canvasAnimation = optionalWord(CANVAS_ANIMATIONS);
export const canvasRoute = word(CANVAS_ROUTES, DEFS.canvas_route.default);
export const canvasStartMark = word(
  CANVAS_MARKS,
  DEFS.canvas_edge.properties.start_mark.default,
);
export const canvasEndMark = word(CANVAS_MARKS, DEFS.canvas_edge.properties.end_mark.default);
export const canvasEdgeAnimation = word(
  CANVAS_EDGE_ANIMATIONS,
  DEFS.canvas_edge_animation.default,
);
export const canvasFlowSetting = optionalWord(CANVAS_FLOWS);
export const canvasPattern = optionalWord(CANVAS_PATTERNS);
export const canvasPictureFit = word(
  CANVAS_PICTURE_FITS,
  DEFS.canvas_picture.properties.fit.default,
);
export const canvasMediaKind = optionalWord(CANVAS_MEDIA_KINDS);

/** The flow and the pattern of a board that states none. */
export const CANVAS_DEFAULT_FLOW = DEFS.canvas_flow.default as CanvasFlowSetting;
export const CANVAS_DEFAULT_PATTERN = DEFS.canvas_pattern.default as CanvasPattern;
/** The largest count that a comment mark shows (`view.json` `canvas_card.comments`). */
export const CANVAS_MAX_COMMENTS = DEFS.canvas_card.properties.comments.maximum;
