import {
  memo,
  useCallback,
  useEffect,
  useLayoutEffect,
  useMemo,
  useRef,
  useState,
  type MouseEvent as ReactMouseEvent,
  type PointerEvent as ReactPointerEvent,
  type ReactNode,
} from 'react';
import {
  CANVAS_FAR_ZOOM,
  canvasFlow,
  canvasFrameHeight,
  canvasLayout,
  canvasTarget,
  canvasZoomedAt,
  type CanvasCamera,
  type CanvasLayout,
} from '../lib/canvas';
import type { LiveCanvasCard, LiveCanvasNode, LiveTone } from '../lib/live-view';
import { distance, midpoint, wheelFactor, type Point, type Size } from '../lib/zoom-pan';
import { InlineMarkdown } from './ChatContent';
import { MinusIcon, PlusIcon } from './icons';
import { Button, IconButton } from './ui';

/** The tallest frame. The style sheet also holds the frame under 70% of the screen. */
const MAX_HEIGHT = 520;
/** The band at the bottom of the frame that the camera bar stands in. */
const BAR_ROOM = 44;
/** The dots of the paper stand this far apart at full size. */
const GRID = 24;
/** A pointer that travelled less than this pressed a card and did not move the board. */
const DRAG_START = 4;
/** One press of a zoom button. */
const ZOOM_STEP = 1.25;

/** The ink of an arrow. A plain arrow takes the ink of a field edge. */
const ARROW_INK: Record<LiveTone, string> = {
  idle: 'text-dialog-hint',
  running: 'text-accent-ink',
  ok: 'text-ok',
  warn: 'text-warn',
  error: 'text-err',
};

/** The edge of a card. A card that waits has a dashed edge on the paper of the page. */
const CARD_EDGE: Record<LiveTone, string> = {
  idle: 'border-dashed border-field-edge bg-ink',
  running: 'border-accent-ink/60 bg-panel',
  ok: 'border-ok/45 bg-panel',
  warn: 'border-warn/60 bg-panel',
  error: 'border-err bg-panel',
};

type Mark = (tone: LiveTone, className?: string) => ReactNode;

/**
 * Safari's own trackpad pinch. No other engine sends it, and the desktop app is a
 * WKWebView, where the ctrl+wheel of the other engines never arrives.
 */
type SafariGestureEvent = Event & { scale: number; clientX: number; clientY: number };

/** A point of the screen, from the top left corner of `frame`. */
function inFrame(frame: HTMLElement | null, point: Point): Point {
  const box = frame?.getBoundingClientRect();
  return box ? { x: point.x - box.left, y: point.y - box.top } : point;
}

function CardFace({
  card,
  sources,
  mark,
}: {
  card: LiveCanvasCard;
  sources: string[];
  mark: Mark;
}) {
  return (
    <>
      <span className="block group-data-[far=true]/canvas:invisible">
        <span className="flex min-w-0 items-center gap-1.5">
          {card.tone && mark(card.tone)}
          <span className="min-w-0 truncate text-body font-semibold text-white">{card.title}</span>
          {card.meta && (
            <span className="ml-auto shrink-0 pl-2 font-mono text-meta tabular-nums text-dialog-hint">
              {card.meta}
            </span>
          )}
        </span>
        {card.text && (
          <span className="mt-1 line-clamp-3 block text-ui text-dialog-foreground [overflow-wrap:anywhere]">
            <InlineMarkdown>{card.text}</InlineMarkdown>
          </span>
        )}
        {card.badges.length > 0 && (
          <span className="mt-1.5 flex flex-wrap gap-1">
            {card.badges.map((badge) => (
              <span
                key={badge}
                className="border border-edge-strong px-1.5 font-mono text-meta text-dialog-hint"
              >
                {badge}
              </span>
            ))}
          </span>
        )}
      </span>
      {sources.length > 0 && <span className="sr-only">After {sources.join(', ')}</span>}
      <span
        aria-hidden="true"
        className="absolute inset-0 hidden items-center gap-3 px-3 group-data-[far=true]/canvas:flex"
      >
        {card.tone && mark(card.tone, 'size-6')}
        <span className="min-w-0 truncate text-display font-semibold text-white">{card.title}</span>
      </span>
    </>
  );
}

/**
 * The cards and the arrows at full size. The camera moves the element around this
 * board, so a pan or a zoom draws none of it again.
 */
const Board = memo(function Board({
  node,
  layout,
  mark,
  onSelect,
}: {
  node: LiveCanvasNode;
  layout: CanvasLayout;
  mark: Mark;
  onSelect?: (nodeId: string, itemIds: string[]) => void;
}) {
  const titles = new Map(node.cards.map((card) => [card.id, card.title]));
  const boxes = new Map(layout.boxes.map((box) => [box.id, box]));
  const selected = new Set(node.selected_ids);
  const isSelectable = node.is_selectable && Boolean(onSelect);
  return (
    <>
      <svg
        aria-hidden="true"
        className="pointer-events-none absolute left-0 top-0 overflow-visible"
        width={layout.width}
        height={layout.height}
      >
        {layout.arrows.map(({ id, path, head, middle, edge }) => (
          <g
            key={id}
            data-canvas-arrow={id}
            className={edge.tone ? ARROW_INK[edge.tone] : 'text-field-edge'}
          >
            <path
              d={path}
              className={`fill-none stroke-current [stroke-width:1.25] [vector-effect:non-scaling-stroke] ${
                edge.is_active
                  ? 'animate-canvas-flow [stroke-dasharray:4_5] motion-reduce:animate-none'
                  : edge.tone === 'idle'
                    ? '[stroke-dasharray:2_5]'
                    : ''
              }`}
            />
            <polygon points={head} className="fill-current" />
            {edge.is_active && (
              <circle r="2.5" className="fill-current motion-reduce:hidden">
                <animateMotion dur="1.6s" repeatCount="indefinite" path={path} />
              </circle>
            )}
            {edge.label && (
              <text
                x={middle.x}
                y={middle.y}
                textAnchor="middle"
                dominantBaseline="central"
                className="fill-dialog-hint stroke-ink font-mono text-meta [paint-order:stroke] [stroke-linejoin:round] [stroke-width:5px] group-data-[far=true]/canvas:hidden"
              >
                {edge.label}
              </text>
            )}
          </g>
        ))}
      </svg>
      {node.cards.map((card) => {
        const box = boxes.get(card.id);
        if (!box) return null;
        const isSelected = selected.has(card.id);
        const sources = card.edges
          .filter((edge) => titles.has(edge.from))
          .map((edge) => `${titles.get(edge.from)}${edge.label ? ` (${edge.label})` : ''}`);
        const face = `absolute overflow-hidden border px-2.5 py-2 text-left ${
          card.tone ? CARD_EDGE[card.tone] : 'border-edge-strong bg-panel'
        } ${isSelected ? 'outline outline-2 -outline-offset-1 outline-accent' : ''}`;
        const place = { left: box.x, top: box.y, width: box.width };
        const body = <CardFace card={card} sources={sources} mark={mark} />;
        return isSelectable ? (
          <button
            key={card.id}
            type="button"
            data-canvas-card={card.id}
            aria-pressed={isSelected}
            className={`${face} cursor-pointer transition-colors duration-150 hover:bg-hover focus-visible:ring-2 focus-visible:ring-accent/60 motion-reduce:transition-none`}
            style={place}
            onClick={() => onSelect?.(node.id, [card.id])}
          >
            {body}
          </button>
        ) : (
          <div key={card.id} data-canvas-card={card.id} className={face} style={place}>
            {body}
          </div>
        );
      })}
    </>
  );
});

/**
 * A live `canvas`: cards and arrows on a board with a camera. The owner points the
 * camera with `zoom` and `focus`. A person who moves the board holds the camera until
 * the owner points it again or the person presses Fit.
 *
 * The camera is written to style and not to state: a pinch that drew React again on
 * each frame stutters on the devices that pinch.
 */
export function LiveCanvas({
  node,
  mark,
  onSelect,
}: {
  node: LiveCanvasNode;
  /** The mark of a tone, with its word for a screen reader. */
  mark: Mark;
  onSelect?: (nodeId: string, itemIds: string[]) => void;
}) {
  const frame = useRef<HTMLDivElement>(null);
  const world = useRef<HTMLDivElement>(null);
  const label = useRef<HTMLSpanElement>(null);
  const camera = useRef<CanvasCamera>({ x: 0, y: 0, scale: 1 });
  const isHeld = useRef(false);
  const canGlide = useRef(false);
  const pointers = useRef(new Map<number, Point>());
  const gesture = useRef({ hasMoved: false, spread: 0 });
  const [size, setSize] = useState<Size>({ width: 0, height: 0 });
  const [heights, setHeights] = useState<ReadonlyMap<string, number>>(new Map());

  const flow = canvasFlow(size.width);
  const layout = useMemo(
    () => canvasLayout(node.cards, heights, flow),
    [node.cards, heights, flow],
  );
  const isLocked = node.is_locked;
  // The board stands in the part of the frame above the camera bar.
  const barRoom = isLocked ? 0 : BAR_ROOM;
  const room = useMemo(
    () => ({ width: size.width, height: Math.max(0, size.height - barRoom) }),
    [size, barRoom],
  );
  const target = useMemo(
    () => canvasTarget(layout, room, node.zoom, node.focus),
    [layout, room, node.zoom, node.focus],
  );
  const latest = useRef({ layout, target, room, isLocked });
  useLayoutEffect(() => {
    latest.current = { layout, target, room, isLocked };
  });

  const paint = useCallback((isGliding: boolean) => {
    const frameNode = frame.current;
    const worldNode = world.current;
    if (!frameNode || !worldNode) return;
    const { x, y, scale } = camera.current;
    const duration = isGliding ? '' : '0ms';
    worldNode.style.transitionDuration = duration;
    worldNode.style.transform = `translate(${x}px, ${y}px) scale(${scale})`;
    let grid = GRID * scale;
    while (grid < GRID / 2) grid *= 2;
    frameNode.style.transitionDuration = duration;
    frameNode.style.backgroundSize = `${grid}px ${grid}px`;
    frameNode.style.backgroundPosition = `${x}px ${y}px`;
    frameNode.dataset.far = String(scale < CANVAS_FAR_ZOOM);
    // A person who holds the camera moves the board with one finger. Before that,
    // one finger scrolls the page, so a board in a long page is not a trap.
    frameNode.style.touchAction =
      latest.current.isLocked ? '' : isHeld.current ? 'none' : 'pan-x pan-y';
    if (label.current) label.current.textContent = `${Math.round(scale * 100)}%`;
  }, []);

  /** The person moves the camera and holds it. */
  const move = useCallback(
    (next: CanvasCamera, isGliding = false) => {
      isHeld.current = true;
      camera.current = next;
      paint(isGliding);
    },
    [paint],
  );

  const zoomBy = (factor: number) => {
    const { width, height } = latest.current.room;
    move(canvasZoomedAt(camera.current, factor, { x: width / 2, y: height / 2 }), true);
  };

  const fit = () => {
    isHeld.current = false;
    camera.current = latest.current.target;
    paint(true);
  };

  // The owner points the camera: with a new `zoom`, `focus` or lock the person's hold ends.
  const pointed = `${node.zoom}:${node.focus}:${isLocked}`;
  const seenPointed = useRef(pointed);
  useLayoutEffect(() => {
    if (seenPointed.current !== pointed) {
      seenPointed.current = pointed;
      isHeld.current = false;
    }
    if (isHeld.current) return;
    camera.current = target;
    paint(canGlide.current);
  }, [target, pointed, paint]);

  // The first picture stands at once. Each later move of the owner glides.
  const isSized = size.width > 0;
  useEffect(() => {
    if (!isSized) return;
    const frameId = requestAnimationFrame(() => {
      canGlide.current = true;
    });
    return () => cancelAnimationFrame(frameId);
  }, [isSized]);

  useLayoutEffect(() => {
    const node = frame.current;
    if (!node) return;
    const measure = () =>
      setSize((held) =>
        held.width === node.clientWidth && held.height === node.clientHeight
          ? held
          : { width: node.clientWidth, height: node.clientHeight },
      );
    measure();
    if (typeof ResizeObserver === 'undefined') return;
    const observer = new ResizeObserver(measure);
    observer.observe(node);
    return () => observer.disconnect();
  }, []);

  // The height of a card is the height of its words, so the layout asks the browser.
  const measureCards = useCallback(() => {
    const cards = world.current?.querySelectorAll<HTMLElement>('[data-canvas-card]') ?? [];
    setHeights((held) => {
      const next = new Map<string, number>();
      for (const card of cards) {
        if (card.offsetHeight > 0) next.set(card.dataset.canvasCard ?? '', card.offsetHeight);
      }
      const isSame =
        next.size === held.size && [...next].every(([id, height]) => held.get(id) === height);
      return isSame ? held : next;
    });
  }, []);
  useLayoutEffect(measureCards, [measureCards, node.cards]);
  useEffect(() => {
    let isMounted = true;
    document.fonts?.ready.then(() => isMounted && measureCards());
    return () => {
      isMounted = false;
    };
  }, [measureCards]);

  function onPointerDown(event: ReactPointerEvent<HTMLDivElement>) {
    gesture.current.hasMoved = false;
    if (isLocked || (event.pointerType === 'mouse' && event.button !== 0)) return;
    if ((event.target as Element).closest('[data-canvas-bar]')) return;
    pointers.current.set(event.pointerId, { x: event.clientX, y: event.clientY });
    gesture.current.spread = 0;
  }

  function onPointerMove(event: ReactPointerEvent<HTMLDivElement>) {
    const from = pointers.current.get(event.pointerId);
    if (!from) return;
    const point = { x: event.clientX, y: event.clientY };
    const state = gesture.current;
    if (pointers.current.size === 1) {
      // One finger moves the board only for a person who holds the camera.
      if (event.pointerType === 'touch' && !isHeld.current) return;
      if (!state.hasMoved) {
        if (distance(from, point) < DRAG_START) return;
        state.hasMoved = true;
        event.currentTarget.setPointerCapture(event.pointerId);
        event.currentTarget.dataset.dragging = 'true';
      }
      pointers.current.set(event.pointerId, point);
      const { x, y, scale } = camera.current;
      move({ x: x + point.x - from.x, y: y + point.y - from.y, scale });
      return;
    }
    pointers.current.set(event.pointerId, point);
    const [a, b] = [...pointers.current.values()];
    const spread = Math.max(1, distance(a, b));
    if (state.spread) {
      move(canvasZoomedAt(camera.current, spread / state.spread, inFrame(frame.current, midpoint(a, b))));
    }
    state.spread = spread;
    state.hasMoved = true;
  }

  function onPointerUp(event: ReactPointerEvent<HTMLDivElement>) {
    pointers.current.delete(event.pointerId);
    gesture.current.spread = 0;
    if (pointers.current.size === 0) delete event.currentTarget.dataset.dragging;
  }

  /** The click that ends a drag belongs to the drag and not to the card under it. */
  function onClickCapture(event: ReactMouseEvent<HTMLDivElement>) {
    if (!gesture.current.hasMoved) return;
    gesture.current.hasMoved = false;
    event.preventDefault();
    event.stopPropagation();
  }

  function onDoubleClick(event: ReactMouseEvent<HTMLDivElement>) {
    if (isLocked) return;
    const card = (event.target as Element).closest<HTMLElement>('[data-canvas-card]');
    const id = card?.dataset.canvasCard;
    if (id) move(canvasTarget(latest.current.layout, latest.current.room, 1, id), true);
  }

  // React listens to the wheel passively, so these listeners are native. A plain
  // wheel stays with the page. A pinch on a trackpad zooms about the pointer.
  useEffect(() => {
    const node = frame.current;
    if (!node) return;
    const onWheel = (event: WheelEvent) => {
      if (latest.current.isLocked || !(event.ctrlKey || event.metaKey)) return;
      event.preventDefault();
      move(
        canvasZoomedAt(
          camera.current,
          wheelFactor(event.deltaY, event.deltaMode, event.ctrlKey),
          inFrame(node, { x: event.clientX, y: event.clientY }),
        ),
      );
    };
    let base: CanvasCamera | null = null;
    let baseScale = 1;
    const onGestureStart = (event: SafariGestureEvent) => {
      if (latest.current.isLocked) return;
      event.preventDefault();
      base = { ...camera.current };
      baseScale = event.scale || 1;
    };
    const onGestureChange = (event: SafariGestureEvent) => {
      if (!base) return;
      event.preventDefault();
      move(
        canvasZoomedAt(
          base,
          (event.scale || 1) / baseScale,
          inFrame(node, { x: event.clientX, y: event.clientY }),
        ),
      );
    };
    const onGestureEnd = () => {
      base = null;
    };
    // Two fingers are a pinch of the board and never a scroll of the page.
    const onTouchMove = (event: TouchEvent) => {
      if (!latest.current.isLocked && event.touches.length > 1 && event.cancelable) {
        event.preventDefault();
      }
    };
    const listeners: [string, (event: never) => void][] = [
      ['wheel', onWheel],
      ['gesturestart', onGestureStart],
      ['gesturechange', onGestureChange],
      ['gestureend', onGestureEnd],
      ['touchmove', onTouchMove],
    ];
    for (const [type, listener] of listeners) {
      node.addEventListener(type, listener as EventListener, { passive: false });
    }
    return () => {
      for (const [type, listener] of listeners) {
        node.removeEventListener(type, listener as EventListener);
      }
    };
  }, [move]);

  const summary = `Canvas, ${node.cards.length} ${node.cards.length === 1 ? 'card' : 'cards'}`;
  const glide = 'duration-300 ease-out motion-reduce:transition-none';
  return (
    <div
      ref={frame}
      role="group"
      aria-label={node.label || summary}
      data-canvas={node.id}
      data-far="false"
      className={`group/canvas relative max-h-[70vh] w-full select-none overflow-hidden border border-edge-strong bg-ink transition-[background-position,background-size] ${glide} ${
        isLocked ? '' : 'cursor-grab data-[dragging=true]:cursor-grabbing'
      }`}
      style={{
        height: canvasFrameHeight(layout, size.width, MAX_HEIGHT - barRoom) + barRoom,
        backgroundImage: 'radial-gradient(var(--line2) 1px, transparent 1px)',
      }}
      onPointerDown={onPointerDown}
      onPointerMove={onPointerMove}
      onPointerUp={onPointerUp}
      onPointerCancel={onPointerUp}
      onClickCapture={onClickCapture}
      onDoubleClick={onDoubleClick}
    >
      <div
        ref={world}
        className={`absolute left-0 top-0 origin-top-left transition-transform ${glide}`}
        style={{ width: layout.width, height: layout.height }}
      >
        <Board node={node} layout={layout} mark={mark} onSelect={onSelect} />
      </div>
      {!isLocked && (
        <div
          data-canvas-bar=""
          role="group"
          aria-label="Canvas camera"
          className="absolute bottom-2 right-2 flex cursor-default items-center border border-edge-strong bg-ink"
        >
          <IconButton label="Zoom out" title="Zoom out" onClick={() => zoomBy(1 / ZOOM_STEP)}>
            <MinusIcon className="size-3" />
          </IconButton>
          <Button
            variant="quiet"
            density="inline"
            pressEffect="none"
            className="min-w-11 tabular-nums"
            aria-label="Full size"
            title="Full size"
            onClick={() => zoomBy(1 / camera.current.scale)}
          >
            <span ref={label}>100%</span>
          </Button>
          <IconButton label="Zoom in" title="Zoom in" onClick={() => zoomBy(ZOOM_STEP)}>
            <PlusIcon className="size-3" />
          </IconButton>
          <Button
            variant="quiet"
            density="inline"
            pressEffect="none"
            aria-label="Fit the board"
            title="Fit the board"
            onClick={fit}
          >
            Fit
          </Button>
        </div>
      )}
    </div>
  );
}
