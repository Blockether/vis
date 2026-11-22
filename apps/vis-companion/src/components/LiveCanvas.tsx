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
  type CanvasMeasure,
} from '../lib/canvas';
import type { CanvasPattern } from '../lib/canvas-vocabulary';
import type { LiveCanvasNode, LiveThread } from '../lib/live-view';
import { distance, midpoint, wheelFactor, type Point, type Size } from '../lib/zoom-pan';
import { CanvasArrowLabels, CanvasArrows } from './CanvasArrows';
import type { AppNodeActions } from './AppNodes';
import { CanvasItem, type CanvasTalkState, type CanvasToneMark } from './CanvasItem';
import { CanvasThreads } from './CanvasThreads';
import { AppIcon, MinusIcon, PlusIcon } from './icons';
import { Button, IconButton } from './ui';

/** The tallest frame. The style sheet also holds the frame under 70% of the screen. */
const MAX_HEIGHT = 640;
/** The band at the bottom of the frame that the camera bar stands in. */
const BAR_ROOM = 44;
/** The marks of the paper stand this far apart at full size. */
const GRID = 24;
/** A pointer that travelled less than this pressed a card and did not move the board. */
const DRAG_START = 4;
/** One press of a zoom button. */
const ZOOM_STEP = 1.25;

/** The paper of the board for each pattern. `plain` has none. */
const PAPER: Record<CanvasPattern, string | undefined> = {
  dots: 'radial-gradient(var(--line2) 1px, transparent 1px)',
  grid: 'linear-gradient(var(--line) 1px, transparent 1px), linear-gradient(90deg, var(--line) 1px, transparent 1px)',
  lines: 'linear-gradient(var(--line) 1px, transparent 1px)',
  plain: undefined,
};

/**
 * Safari's own trackpad pinch. No other engine sends it, and the desktop app is a
 * WKWebView, where the ctrl+wheel of the other engines never arrives.
 */
type SafariGestureEvent = Event & { scale: number; clientX: number; clientY: number };

/** A frame under this width shows the discussions as a sheet over its lower part. */
const SHEET_WIDTH = 560;

/** What the threads of one item hold, for the mark at its corner. */
interface Talk {
  count: number;
  state: CanvasTalkState;
}

/** The mark of each item that has a thread: a saved draft shows before open messages. */
function talksOf(threads: readonly LiveThread[]): ReadonlyMap<string, Talk> {
  const talks = new Map<string, Talk>();
  for (const thread of threads) {
    if (!thread.item_id) continue;
    const hasDraft = thread.draft !== undefined;
    const state: CanvasTalkState = hasDraft ? 'draft' : thread.is_resolved ? 'resolved' : 'open';
    const count = thread.messages.length + (hasDraft && thread.draft !== '' ? 1 : 0);
    const held = talks.get(thread.item_id);
    talks.set(thread.item_id, {
      count: (held?.count ?? 0) + count,
      state:
        held?.state === 'draft' || state === 'draft'
          ? 'draft'
          : held?.state === 'open' || state === 'open'
            ? 'open'
            : 'resolved',
    });
  }
  return talks;
}

/** A point of the screen, from the top left corner of `frame`. */
function inFrame(frame: HTMLElement | null, point: Point): Point {
  const box = frame?.getBoundingClientRect();
  return box ? { x: point.x - box.left, y: point.y - box.top } : point;
}

/**
 * The items and the arrows at full size. The camera moves the element around this
 * board, so a pan or a zoom draws none of it again. The frames stand under the
 * arrows, the other items over them, and the words of the arrows over everything.
 */
const Board = memo(function Board({
  node,
  layout,
  mark,
  talks,
  talkingId,
  canComment,
  onSelect,
  onTalk,
}: {
  node: LiveCanvasNode;
  layout: CanvasLayout;
  mark: CanvasToneMark;
  talks: ReadonlyMap<string, Talk>;
  /** The item whose discussion is open. */
  talkingId: string | null;
  /** False on a board that a person only reads: an item without a thread has no mark. */
  canComment: boolean;
  onSelect?: (itemId: string) => void;
  /** Absent when a person can neither read nor write a comment. */
  onTalk?: (itemId: string) => void;
}) {
  const cards = new Map(node.cards.map((card) => [card.id, card]));
  const selected = new Set(node.selected_ids);
  const items = (isFrame: boolean) =>
    layout.boxes.map((box) => {
      const card = cards.get(box.id);
      if (!card || (card.kind === 'frame') !== isFrame) return null;
      const after = card.edges
        .filter((edge) => cards.has(edge.from) && edge.line !== 'none')
        .map((edge) => `${cards.get(edge.from)?.title}${edge.label ? ` (${edge.label})` : ''}`)
        .join(', ');
      const talk = talks.get(card.id);
      return (
        <CanvasItem
          key={card.id}
          card={card}
          box={box}
          mark={mark}
          after={after}
          isSelected={selected.has(card.id)}
          onSelect={onSelect}
          talkCount={talk?.count ?? 0}
          talkState={talk?.state}
          isTalkOpen={talkingId === card.id}
          onTalk={talk || canComment ? onTalk : undefined}
        />
      );
    });
  return (
    <>
      {items(true)}
      <CanvasArrows layout={layout} />
      {items(false)}
      <CanvasArrowLabels layout={layout} />
    </>
  );
});

/**
 * A live `canvas`: cards and arrows on a board with a camera. The owner points the
 * camera with `zoom` and `focus`. A person who moves the board holds the camera until
 * the owner points it again or the person presses Fit. The threads of the items
 * open in a panel over the board, and with an app a person writes comments there.
 *
 * The camera is written to style and not to state: a pinch that drew React again on
 * each frame stutters on the devices that pinch.
 */
export function LiveCanvas({
  node,
  mark,
  onSelect,
  app,
}: {
  node: LiveCanvasNode;
  /** The mark of a tone, with its word for a screen reader. */
  mark: CanvasToneMark;
  onSelect?: (nodeId: string, itemIds: string[]) => void;
  /** The Save action of an app. Absent, the threads are read only. */
  app?: AppNodeActions;
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
  const [measures, setMeasures] = useState<ReadonlyMap<string, CanvasMeasure>>(new Map());
  /** The open panel: one item, or the whole board with a null id. */
  const [talk, setTalk] = useState<{ itemId: string | null; isComposing: boolean } | null>(null);
  /** The person adds comments: each item shows the control that starts one. */
  const [isCommenting, setCommenting] = useState(false);

  const canComment = Boolean(app) && !app?.isReadOnly;
  const talks = useMemo(() => talksOf(node.threads), [node.threads]);
  const hasTalk = canComment || node.threads.length > 0;
  const nodeId = node.id;
  const isSelectable = node.is_selectable && Boolean(onSelect);
  const select = useMemo(
    () => (isSelectable ? (itemId: string) => onSelect?.(nodeId, [itemId]) : undefined),
    [isSelectable, onSelect, nodeId],
  );
  const openTalk = useCallback(
    (itemId: string) => {
      setTalk({ itemId, isComposing: canComment && !talks.has(itemId) });
      setCommenting(false);
    },
    [canComment, talks],
  );

  const flow = canvasFlow(size.width, node.flow);
  const layout = useMemo(
    () => canvasLayout(node.cards, measures, flow),
    [node.cards, measures, flow],
  );
  const isLocked = node.is_locked;
  // The board stands in the part of the frame above the bar.
  const barRoom = isLocked && !hasTalk ? 0 : BAR_ROOM;
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
    // A far camera makes each line thin. The arrows grow against the scale, so a
    // line stays near one pixel on the screen.
    frameNode.style.setProperty('--cv-weight', String(Math.min(2.2, Math.max(1, 0.8 / scale))));
    // The mark of a discussion keeps its size on the screen, so a person can read
    // and press it on a board that shows whole.
    frameNode.style.setProperty('--cv-mark', String(Math.min(3, Math.max(1, 1 / scale))));
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

  /** The whole board, also when the owner points the camera at one item. */
  const fit = () => {
    const { layout, room, target } = latest.current;
    const whole = canvasTarget(layout, room, 0, '');
    // The person holds the camera only when the owner points it at another place.
    isHeld.current =
      whole.x !== target.x || whole.y !== target.y || whole.scale !== target.scale;
    camera.current = whole;
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

  // The height of an item is the height of its words, so the layout asks the browser.
  // A frame is as high as the layout makes it: only its header has a height of its own.
  const measureCards = useCallback(() => {
    const cards = world.current?.querySelectorAll<HTMLElement>('[data-canvas-card]') ?? [];
    setMeasures((held) => {
      const next = new Map<string, CanvasMeasure>();
      for (const card of cards) {
        const body =
          card.dataset.canvasKind === 'frame'
            ? card.querySelector<HTMLElement>('[data-canvas-head]')
            : card;
        const caption = card.parentElement?.querySelector<HTMLElement>(':scope > [data-canvas-caption]');
        if (!body || body.offsetHeight <= 0) continue;
        next.set(card.dataset.canvasCard ?? '', {
          height: body.offsetHeight,
          ...(caption ? { caption: caption.offsetHeight } : {}),
        });
      }
      const isSame =
        next.size === held.size &&
        [...next].every(([id, { height, caption }]) => {
          const one = held.get(id);
          return one?.height === height && one.caption === caption;
        });
      return isSame ? held : next;
    });
  }, []);
  useLayoutEffect(measureCards, [measureCards, node.cards, flow]);
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
    // A press on the camera bar or on a player is not a move of the board.
    if ((event.target as Element).closest('[data-canvas-bar], [data-canvas-control]')) return;
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

  const summary = `Canvas, ${node.cards.length} ${node.cards.length === 1 ? 'item' : 'items'}`;
  const glide = 'duration-300 ease-out motion-reduce:transition-none';
  return (
    <div
      ref={frame}
      role="group"
      aria-label={node.label || summary}
      data-canvas={node.id}
      data-far="false"
      data-commenting={isCommenting}
      className={`group/canvas relative max-h-[70vh] w-full select-none overflow-hidden border border-edge-strong bg-ink transition-[background-position,background-size] ${glide} ${
        isLocked ? '' : 'cursor-grab data-[dragging=true]:cursor-grabbing'
      }`}
      style={{
        height: canvasFrameHeight(layout, size.width, MAX_HEIGHT - barRoom) + barRoom,
        backgroundImage: PAPER[node.pattern],
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
        <Board
          node={node}
          layout={layout}
          mark={mark}
          talks={talks}
          talkingId={talk?.itemId ?? null}
          canComment={canComment}
          onSelect={select}
          onTalk={hasTalk ? openTalk : undefined}
        />
      </div>
      {talk && (
        <CanvasThreads
          node={node}
          itemId={talk.itemId}
          isSheet={size.width < SHEET_WIDTH}
          isComposing={talk.isComposing}
          actions={app}
          onCompose={(isComposing) => setTalk((held) => held && { ...held, isComposing })}
          onOpen={(itemId) => {
            setTalk({ itemId, isComposing: false });
            if (itemId !== null && !isLocked) {
              move(canvasTarget(latest.current.layout, latest.current.room, camera.current.scale, itemId), true);
            }
          }}
          onClose={() => setTalk(null)}
        />
      )}
      {hasTalk && (
        <div
          data-canvas-bar=""
          role="group"
          aria-label="Canvas comments"
          className="absolute bottom-2 left-2 flex cursor-default items-center border border-edge-strong bg-ink"
        >
          <Button
            variant="quiet"
            density="compact"
            pressEffect="none"
            className="inline-flex min-w-11 items-center justify-center gap-1.5 tabular-nums"
            aria-label={`Comments, ${node.threads.length} ${node.threads.length === 1 ? 'thread' : 'threads'}`}
            title="Comments"
            aria-pressed={talk !== null}
            onClick={() => setTalk((held) => (held ? null : { itemId: null, isComposing: false }))}
          >
            <AppIcon name="chat" className="size-3" />
            {node.threads.length}
          </Button>
          {canComment && (
            <Button
              variant="quiet"
              density="compact"
              pressEffect="none"
              className="inline-flex min-w-11 items-center justify-center gap-1.5"
              aria-label="Comment on items"
              title="Show the comment control on each item"
              aria-pressed={isCommenting}
              onClick={() => setCommenting((held) => !held)}
            >
              <PlusIcon className="size-3" />
              Comment
            </Button>
          )}
        </div>
      )}
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
            density="compact"
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
            density="compact"
            pressEffect="none"
            className="min-w-11"
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
