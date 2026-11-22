/**
 * The arrows of a live `canvas`. The layout gives each arrow its line and the place
 * of its two marks. This file gives them their ink, their line and their motion.
 */

import { useEffect, useRef } from 'react';
import { canvasMarkPath, type CanvasArrow, type CanvasLayout } from '../lib/canvas';
import type { CanvasLine, CanvasMark } from '../lib/canvas-vocabulary';
import type { LiveCanvasEdge, LiveTone } from '../lib/live-view';

/** The ink of an arrow with a tone. A plain arrow takes the ink of a field edge. */
const TONE_INK: Record<LiveTone, string> = {
  idle: 'text-dialog-hint',
  running: 'text-accent-ink',
  ok: 'text-ok',
  warn: 'text-warn',
  error: 'text-err',
};

const LINE: Record<CanvasLine, { width: number; dash?: string; isRound?: boolean }> = {
  solid: { width: 1.25 },
  dashed: { width: 1.25, dash: '6 4' },
  dotted: { width: 1.75, dash: '0.01 5', isRound: true },
  double: { width: 4 },
  thick: { width: 2.5 },
  none: { width: 0 },
};

/** The width of a line. The board sets `--cv-weight` above 1 for a far camera. */
const weight = (width: number) => ({ strokeWidth: `calc(${width}px * var(--cv-weight, 1))` });

/** The marks that are a filled figure. An arrow and a bar are lines only. */
const FILLED: ReadonlySet<CanvasMark> = new Set(['triangle', 'circle', 'diamond']);

/** The dashes of an arrow with work on it. One period is 9, as `canvas-flow` moves. */
const WORK_DASH = '4 5';

function ink(edge: LiveCanvasEdge): string {
  if (edge.color) return 'text-[color:var(--cv-ink)]';
  return edge.tone ? TONE_INK[edge.tone] : 'text-field-edge';
}

function Mark({ mark, x, y, angle }: { mark: CanvasMark; x: number; y: number; angle: number }) {
  const path = canvasMarkPath(mark);
  if (!path) return null;
  return (
    <path
      d={path}
      transform={`translate(${x} ${y}) rotate(${angle})`}
      className={`stroke-current [stroke-linecap:round] [stroke-linejoin:round] ${
        FILLED.has(mark) ? 'fill-current' : 'fill-none'
      }`}
      style={weight(1.25)}
    />
  );
}

function Arrow({ arrow }: { arrow: CanvasArrow }) {
  const { from, to, path, edge, start, end } = arrow;
  const { width, dash, isRound } = LINE[edge.line];
  const isMoving = edge.is_active && (edge.animation === 'flow' || edge.animation === 'dash');
  const hasDot = edge.is_active && (edge.animation === 'flow' || edge.animation === 'dot');
  const isPulsing = edge.is_active && edge.animation === 'pulse';
  const line = `fill-none ${isMoving ? 'animate-canvas-flow motion-reduce:animate-none' : ''}`;
  const lineDash = isMoving ? WORK_DASH : (dash ?? (edge.tone === 'idle' ? '2 5' : undefined));
  return (
    <g
      data-canvas-arrow={`${from}>${to}`}
      data-canvas-color={edge.color}
      className={`${ink(edge)} ${
        isPulsing
          ? 'animate-canvas-edge-pulse [filter:drop-shadow(0_0_3px_currentColor)] motion-reduce:animate-none'
          : ''
      }`}
    >
      <path
        d={path}
        className={`${line} stroke-current`}
        style={weight(width)}
        strokeDasharray={lineDash}
        strokeLinecap={isRound && !isMoving ? 'round' : undefined}
      />
      {/* The gap of a double line is a thinner line in the colour of the board. */}
      {edge.line === 'double' && (
        <path d={path} className={`${line} stroke-ink`} style={weight(1.5)} strokeDasharray={lineDash} />
      )}
      <Mark mark={edge.start_mark} {...start} />
      <Mark mark={edge.end_mark} {...end} />
      {hasDot && (
        <circle r="2.75" className="fill-current motion-reduce:hidden">
          <animateMotion dur="1.6s" repeatCount="indefinite" path={path} />
        </circle>
      )}
    </g>
  );
}

const LAYER = 'pointer-events-none absolute left-0 top-0 overflow-visible';

/** The lines and the marks. They stand over the frames and under the other items. */
export function CanvasArrows({ layout }: { layout: CanvasLayout }) {
  const layer = useRef<SVGSVGElement>(null);
  const hasMotion = layout.arrows.some(({ edge }) => edge.is_active);
  useEffect(() => {
    const svg = layer.current;
    const frame = svg?.closest('[data-canvas]');
    if (!svg || !frame || !hasMotion) return;
    const preference = window.matchMedia?.('(prefers-reduced-motion: reduce)');
    const canObserve = typeof IntersectionObserver !== 'undefined';
    let isVisible = !canObserve;
    const sync = () => {
      const isPaused = !isVisible || Boolean(preference?.matches);
      svg.toggleAttribute('data-motion-paused', isPaused);
      if (isPaused) svg.pauseAnimations?.();
      else svg.unpauseAnimations?.();
    };
    sync();
    const observer = canObserve ? new IntersectionObserver(([entry]) => {
      isVisible = entry.isIntersecting;
      sync();
    }) : undefined;
    observer?.observe(frame);
    preference?.addEventListener('change', sync);
    return () => {
      observer?.disconnect();
      preference?.removeEventListener('change', sync);
    };
  }, [hasMotion]);
  return (
    <svg
      ref={layer}
      aria-hidden="true"
      className={`${LAYER} [&[data-motion-paused]_*]:[animation-play-state:paused]`}
      width={layout.width}
      height={layout.height}
    >
      {layout.arrows.map((arrow) => (
        <Arrow key={arrow.id} arrow={arrow} />
      ))}
    </svg>
  );
}

/** The words of the arrows. They stand over every item, so a line never cuts a word. */
export function CanvasArrowLabels({ layout }: { layout: CanvasLayout }) {
  const labelled = layout.arrows.filter((arrow) => arrow.edge.label);
  if (labelled.length === 0) return null;
  return (
    <svg
      className={LAYER}
      width={layout.width}
      height={layout.height}
    >
      {labelled.map(({ id, middle, labelWidth = 0, edge }) => (
        <g key={id} role="img" aria-label={edge.label}>
          <title>{edge.label}</title>
          {labelWidth >= 16 && (
            <foreignObject
              aria-hidden="true"
              data-canvas-label={id}
              data-canvas-color={edge.color}
              x={middle.x - labelWidth / 2}
              y={middle.y - 10}
              width={labelWidth}
              height={20}
              className="[[data-canvas][data-far=true]_&]:hidden"
            >
              <div className="flex h-full items-center justify-center">
                <span className={`max-w-full truncate bg-ink px-1 font-mono text-meta ${
                  edge.color ? 'text-[color:var(--cv-ink)]' : 'text-dialog-hint'
                }`}>
                  {edge.label}
                </span>
              </div>
            </foreignObject>
          )}
        </g>
      ))}
    </svg>
  );
}
