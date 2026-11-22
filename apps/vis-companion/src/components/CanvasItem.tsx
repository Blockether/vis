/**
 * The items of a live `canvas`: a card, a shape, a sticky note, a frame and a text.
 * The layout gives each item its box, and the item draws its face in it. The mark of
 * its discussion stands at its corner.
 * The colours come from the `--cv-*` paints of the style sheet.
 */

import { memo, useState, type CSSProperties, type ReactNode } from 'react';

import { pictureFacts, pictureSource } from '../lib/app-image';
import {
  canvasItemMinHeight,
  canvasShapeDetail,
  canvasShapeCorner,
  canvasShapePath,
  type CanvasBox,
} from '../lib/canvas';
import type { CanvasLine, CanvasShape, CanvasSize } from '../lib/canvas-vocabulary';
import type {
  LiveCanvasCard,
  LiveCanvasMedia,
  LiveCanvasPicture,
  LiveTone,
} from '../lib/live-view';
import { Markdown } from './ChatContent';
import { AppIcon, CheckIcon, ImageIcon, PencilIcon, PlayIcon, PlusIcon, SpeakerIcon } from './icons';

/** The mark of a tone, with its word for a screen reader. */
export type CanvasToneMark = (tone: LiveTone, className?: string) => ReactNode;

const TITLE = 'text-[color:var(--cv-title)]';
const TEXT = 'text-[color:var(--cv-text)]';
const HINT = 'text-[color:var(--cv-hint)]';
const INK = 'text-[color:var(--cv-ink)]';
/**
 * The part of an item that a far camera hides: only the title stays readable.
 *
 * The board variants select the frame by its attributes and not as a Tailwind group,
 * and the words fade out instead of becoming invisible. A group variant, and an
 * inherited property such as `visibility`, make the browser style each element of the
 * board again, so a board with many words stops for a large part of a second. Only the
 * controls in the faded words become invisible: they take no press and no keyboard focus.
 * Each control kind has its own rule, because one `:is()` rule styles the board again.
 * The scroll areas of tables and code keep their focus: they do no action, and to hide
 * them styles each word of their code again.
 */
const NEAR =
  '[[data-canvas][data-far=true]_&]:opacity-0 [[data-canvas][data-far=true]_&_a]:invisible [[data-canvas][data-far=true]_&_button]:invisible [[data-canvas][data-far=true]_&_summary]:invisible [[data-canvas][data-far=true]_&_audio]:invisible [[data-canvas][data-far=true]_&_video]:invisible [[data-canvas][data-far=true]_&_[data-canvas-control]]:invisible';
/**
 * The words of an item move in a bounce and in a shake. The border stands still, so
 * each arrow stays on it.
 */
const MOVES =
  'group-data-[motion=bounce]/item:animate-canvas-bounce group-data-[motion=shake]/item:animate-canvas-shake motion-reduce:animate-none';

/** The words of a card or a note by its size. */
const TITLE_SIZE: Record<CanvasSize, string> = {
  small: 'text-ui',
  medium: 'text-body',
  large: 'text-title',
};
const PAD: Record<CanvasSize, string> = {
  small: 'px-2.5 py-2',
  medium: 'px-3 py-2.5',
  large: 'px-4 py-3.5',
};
/** A `text` item is a heading: its size is the size of its words. */
const HEADING_SIZE: Record<CanvasSize, string> = {
  small: 'text-body font-semibold',
  medium: 'text-title font-semibold',
  large: 'text-display font-bold',
};

/** The stroke of a shape outline for each line of a border. */
const OUTLINE: Record<CanvasLine, { width: number; dash?: string; isRound?: boolean }> = {
  solid: { width: 1 },
  dashed: { width: 1, dash: '6 4' },
  dotted: { width: 1.5, dash: '0.01 4', isRound: true },
  double: { width: 3 },
  thick: { width: 2 },
  none: { width: 0 },
};

/**
 * The room between the outline of a shape and its words, as parts of the width: a
 * diamond and an ellipse are narrow at the top, and a slanted side takes room.
 */
const SHAPE_ROOM: Record<CanvasShape, { x: number; top: number; bottom: number }> = {
  rectangle: { x: 0.08, top: 0.06, bottom: 0.06 },
  rounded: { x: 0.08, top: 0.06, bottom: 0.06 },
  pill: { x: 0.12, top: 0.05, bottom: 0.05 },
  ellipse: { x: 0.16, top: 0.1, bottom: 0.1 },
  diamond: { x: 0.24, top: 0.2, bottom: 0.2 },
  hexagon: { x: 0.17, top: 0.07, bottom: 0.07 },
  parallelogram: { x: 0.16, top: 0.06, bottom: 0.06 },
  cylinder: { x: 0.08, top: 0.17, bottom: 0.08 },
  document: { x: 0.08, top: 0.06, bottom: 0.13 },
};

/**
 * A picture. On the top edge of a box it fills the width; a loose picture, in an
 * item with no box edge at the top, has its own border.
 */
function ItemPicture({ picture, isLoose = false }: { picture: LiveCanvasPicture; isLoose?: boolean }) {
  const src = pictureSource(picture);
  const ratio =
    picture.width && picture.height
      ? Math.min(2.4, Math.max(1, picture.width / picture.height))
      : 16 / 9;
  const edge = isLoose ? 'mt-2 w-full border' : 'border-b';
  if (!src) {
    return (
      <span
        className={`flex items-center gap-1.5 ${edge} border-[var(--cv-edge)] px-3 py-2 text-left text-meta ${HINT}`}
      >
        <ImageIcon className="size-3.5 shrink-0" />
        <span className="min-w-0 truncate">{picture.alt}</span>
        <span className="ml-auto shrink-0 font-mono tabular-nums">{pictureFacts(picture)}</span>
      </span>
    );
  }
  return (
    <span
      className={`block overflow-hidden ${edge} border-[var(--cv-edge)] bg-panel-2`}
      style={{ aspectRatio: ratio }}
    >
      <img
        src={src}
        alt={picture.alt}
        draggable={false}
        className={`block h-full w-full ${picture.fit === 'contain' ? 'object-contain' : 'object-cover'}`}
      />
    </span>
  );
}

/**
 * A video or a sound. Before the person presses Play, the item holds only a plate
 * with the words of the media: no request leaves the app.
 */
function ItemMedia({ media }: { media: LiveCanvasMedia }) {
  return <MediaPlayer key={JSON.stringify([media.kind, media.url])} media={media} />;
}

function MediaPlayer({ media }: { media: LiveCanvasMedia }) {
  const [isStarted, setIsStarted] = useState(false);
  const isVideo = media.kind === 'video';
  const frame = `pointer-events-auto mt-2 block w-full border border-[var(--cv-edge)] ${
    isVideo ? 'aspect-video bg-black' : 'h-8'
  }`;
  if (isStarted) {
    return isVideo ? (
      <video
        data-canvas-control=""
        src={media.url}
        aria-label={media.alt}
        className={frame}
        controls
        autoPlay
        playsInline
      />
    ) : (
      <audio data-canvas-control="" src={media.url} aria-label={media.alt} className={frame} controls autoPlay />
    );
  }
  return (
    <button
      type="button"
      data-canvas-control=""
      data-canvas-media={media.kind}
      aria-label={`Play the ${isVideo ? 'video' : 'sound'}: ${media.alt}`}
      className={`${frame} group/media relative flex cursor-pointer items-center text-left ${
        isVideo ? 'justify-center text-[#f5f6f8]' : `gap-2 px-2 ${TEXT}`
      }`}
      onClick={() => setIsStarted(true)}
    >
      <span
        aria-hidden="true"
        className={`flex shrink-0 items-center justify-center border border-current transition-transform duration-150 group-hover/media:scale-110 motion-reduce:transition-none ${
          isVideo ? 'size-9' : 'size-5'
        }`}
      >
        <PlayIcon className={isVideo ? 'size-3.5' : 'size-2.5'} />
      </span>
      <span
        className={
          isVideo
            ? 'absolute inset-x-0 bottom-0 truncate px-2 py-1 text-meta text-[#f5f6f8]/80'
            : 'min-w-0 flex-1 truncate text-meta'
        }
      >
        {media.alt}
      </span>
      {!isVideo && <SpeakerIcon className={`size-3.5 shrink-0 ${HINT}`} />}
    </button>
  );
}

/** The work that is done, as a bar. */
function ItemProgress({ value, className }: { value: number; className: string }) {
  const percent = Math.round(value * 100);
  return (
    <span
      role="progressbar"
      aria-label="Progress"
      aria-valuemin={0}
      aria-valuemax={100}
      aria-valuenow={percent}
      className={`block overflow-hidden bg-[var(--cv-tint)] ${className}`}
    >
      <span className="block h-full bg-[var(--cv-ink)]" style={{ width: `${percent}%` }} />
    </span>
  );
}

/** What the discussion of an item holds: a saved draft, open messages, or only resolved ones. */
export type CanvasTalkState = 'draft' | 'open' | 'resolved';

const TALK_WORD: Record<CanvasTalkState, string> = {
  draft: 'with a saved draft',
  open: 'open',
  resolved: 'resolved',
};

/** The place of the mark of a discussion on a shape. The mark centres 10 px in from its own corner. */
function cornerOf(card: LiveCanvasCard, box: CanvasBox): CSSProperties {
  const at = canvasShapeCorner(card.shape, box.width, box.height);
  return { right: box.width - at.x - 10, top: at.y - 10 };
}

/**
 * The discussion of an item, at its corner. With messages it shows their count and
 * the state by its icon. Without messages it is the control that starts a comment:
 * it shows under a pointer, with the keyboard, and while the person adds comments.
 */
function TalkMark({
  title,
  count,
  state,
  isOpen,
  onOpen,
  corner,
}: {
  title: string;
  count: number;
  state?: CanvasTalkState;
  isOpen: boolean;
  onOpen: () => void;
  /** The corner of a shape: its outline, not the empty corner of its box. */
  corner?: CSSProperties;
}) {
  // The mark grows against the camera around the corner of its item.
  const reach =
    'origin-[calc(100%-0.625rem)_0.625rem] scale-[var(--cv-mark,1)] after:absolute after:-inset-3 after:content-[""] mouse:after:content-none';
  const face = isOpen
    ? 'border-accent bg-accent text-accent-foreground'
    : 'border-[var(--cv-edge)] bg-ink text-[color:var(--cv-ink)] hover:border-accent';
  if (!state) {
    return (
      <button
        type="button"
        data-canvas-talk="add"
        aria-label={`Comment on ${title}`}
        title="Comment"
        aria-expanded={isOpen}
        className={`pointer-events-none absolute -right-2.5 -top-2.5 z-10 flex size-5 cursor-pointer items-center justify-center border opacity-0 transition-opacity duration-150 focus-visible:pointer-events-auto focus-visible:opacity-100 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent [[data-canvas][data-commenting=true]_&]:pointer-events-auto [[data-canvas][data-commenting=true]_&]:opacity-100 mouse:group-hover/item:pointer-events-auto mouse:group-hover/item:opacity-100 motion-reduce:transition-none ${
          isOpen ? 'pointer-events-auto opacity-100' : ''
        } ${face} ${reach}`}
        style={corner}
        onClick={onOpen}
      >
        <PlusIcon className="size-3" />
      </button>
    );
  }
  return (
    <button
      type="button"
      data-canvas-talk={state}
      aria-label={`${count} ${count === 1 ? 'comment' : 'comments'} on ${title}, ${TALK_WORD[state]}`}
      title="Comments"
      aria-expanded={isOpen}
      className={`absolute -right-2.5 -top-2.5 z-10 flex h-5 min-w-5 cursor-pointer items-center gap-1 border px-1 font-mono text-meta font-bold tabular-nums transition-colors duration-150 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent motion-reduce:transition-none ${
        state === 'resolved' && !isOpen ? 'border-dashed' : ''
      } ${face} ${reach}`}
      style={corner}
      onClick={onOpen}
    >
      {state === 'resolved' ? (
        <CheckIcon className="size-3" />
      ) : state === 'draft' ? (
        <PencilIcon className="size-3" />
      ) : (
        <AppIcon name="chat" className="size-3" />
      )}
      {count}
    </button>
  );
}

function Badges({ badges }: { badges: string[] }) {
  if (badges.length === 0) return null;
  return (
    <span className="mt-2 flex flex-wrap gap-1">
      {badges.map((badge) => (
        <span
          key={badge}
          className={`min-w-0 max-w-full border border-[var(--cv-edge)] px-1.5 font-mono text-meta [overflow-wrap:anywhere] ${HINT}`}
        >
          {badge}
        </span>
      ))}
    </span>
  );
}

/**
 * The one line that a far camera can still read. It lies over the item and takes no
 * room, so the item keeps the height that the layout measured.
 */
function FarTitle({
  card,
  mark,
  place = 'px-3',
  titleClass = TITLE,
}: {
  card: LiveCanvasCard;
  mark: CanvasToneMark;
  place?: string;
  titleClass?: string;
}) {
  // A shape and a small item are narrow: smaller words keep the title whole.
  const isTight = card.kind === 'shape' || card.size === 'small';
  return (
    <span
      aria-hidden="true"
      className={`absolute inset-0 hidden items-center overflow-hidden [[data-canvas][data-far=true]_&]:flex ${isTight ? 'gap-2' : 'gap-3'} ${place}`}
    >
      {card.icon && <AppIcon name={card.icon} className={`shrink-0 ${isTight ? 'size-5' : 'size-6'} ${INK}`} />}
      {card.tone && mark(card.tone, isTight ? 'size-5' : 'size-6')}
      <span
        className={`min-w-0 truncate font-semibold ${isTight ? 'text-[18px] leading-[22px]' : 'text-display'} ${titleClass}`}
      >
        {card.title}
      </span>
    </span>
  );
}

function ItemMeta({ meta, className }: { meta: string; className: string }) {
  return <span className={`min-w-0 truncate font-mono text-meta tabular-nums ${HINT} ${className}`}>{meta}</span>;
}

/** The title line of a card, a note or a frame: icon, tone, title, meta. */
function Heading({
  card,
  mark,
  titleClass,
}: {
  card: LiveCanvasCard;
  mark: CanvasToneMark;
  titleClass: string;
}) {
  return (
    <span className="flex min-w-0 items-center gap-1.5">
      {card.icon && <AppIcon name={card.icon} className={`size-3.5 shrink-0 ${INK}`} />}
      {card.tone && mark(card.tone)}
      <span className={`min-w-0 truncate font-semibold ${titleClass}`}>{card.title}</span>
      {card.meta && <ItemMeta meta={card.meta} className="ml-auto max-w-[45%] pl-2" />}
    </span>
  );
}

/**
 * The body of an item: Markdown, rendered as a message of the session. The item lets
 * a press go through to the board, but a link or a button in the body takes it.
 */
function ItemText({ text, className = TEXT }: { text: string; className?: string }) {
  return (
    <div
      data-canvas-text=""
      className={`mt-1 min-w-0 [overflow-wrap:anywhere] [&_a]:pointer-events-auto [&_button]:pointer-events-auto ${className}`}
    >
      <Markdown compact nested>
        {text}
      </Markdown>
    </div>
  );
}

function CardFace({ card, mark }: { card: LiveCanvasCard; mark: CanvasToneMark }) {
  return (
    <>
      {/* A picture reads at each distance, so a far camera keeps it. */}
      {card.picture && <ItemPicture picture={card.picture} />}
      <span className="relative block">
        <span className={`block ${PAD[card.size]} ${NEAR} ${MOVES}`}>
          <Heading card={card} mark={mark} titleClass={`${TITLE_SIZE[card.size]} ${TITLE}`} />
          {card.text && <ItemText text={card.text} />}
          {card.media && <ItemMedia media={card.media} />}
          <Badges badges={card.badges} />
        </span>
        <FarTitle card={card} mark={mark} />
      </span>
      {card.progress !== undefined && (
        <span className="absolute inset-x-0 bottom-0 block">
          <ItemProgress value={card.progress} className="h-[3px]" />
        </span>
      )}
    </>
  );
}

/** A sticky note: a sheet of the colour with a folded corner. */
function NoteFace({ card, mark }: { card: LiveCanvasCard; mark: CanvasToneMark }) {
  const fold = 14;
  return (
    <>
      <span
        aria-hidden="true"
        className="absolute inset-0 bg-[var(--cv-fill)]"
        style={{
          clipPath: `polygon(0 0, 100% 0, 100% calc(100% - ${fold}px), calc(100% - ${fold}px) 100%, 0 100%)`,
        }}
      />
      <span
        aria-hidden="true"
        className="absolute bottom-0 right-0 bg-[color-mix(in_oklab,var(--cv-fill)_72%,var(--fg))]"
        style={{ width: fold, height: fold, clipPath: 'polygon(0 0, 100% 0, 0 100%)' }}
      />
      <span className={`relative block ${NEAR}`}>
        {card.picture && <ItemPicture picture={card.picture} />}
        <span className={`block ${PAD[card.size]} pb-4 ${MOVES}`}>
          <Heading card={card} mark={mark} titleClass={`${TITLE_SIZE[card.size]} ${TITLE}`} />
          {card.text && <ItemText text={card.text} />}
          {card.media && <ItemMedia media={card.media} />}
          <Badges badges={card.badges} />
          {card.progress !== undefined && <ItemProgress value={card.progress} className="mt-2 h-[3px]" />}
        </span>
      </span>
      <FarTitle card={card} mark={mark} />
    </>
  );
}

/** A heading on the board: words with no border. */
function TextFace({ card }: { card: LiveCanvasCard }) {
  return (
    <span className={`block ${MOVES}`}>
      <span className="flex min-w-0 items-center gap-2">
        {card.icon && <AppIcon name={card.icon} className={`size-4 shrink-0 ${INK}`} />}
        <span
          className={`min-w-0 [overflow-wrap:anywhere] ${HEADING_SIZE[card.size]} ${card.color ? INK : TITLE}`}
        >
          {card.title}
        </span>
        {card.meta && <ItemMeta meta={card.meta} className={`ml-auto text-right ${NEAR}`} />}
      </span>
      {card.text && <ItemText text={card.text} className={`${HINT} ${NEAR}`} />}
      {(card.picture || card.media) && (
        <span className={`block ${NEAR}`}>
          {card.picture && <ItemPicture picture={card.picture} isLoose />}
          {card.media && <ItemMedia media={card.media} />}
        </span>
      )}
      {(card.badges.length > 0 || card.progress !== undefined) && (
        <span className={`block ${NEAR}`}>
          <Badges badges={card.badges} />
          {card.progress !== undefined && <ItemProgress value={card.progress} className="mt-2 h-[3px]" />}
        </span>
      )}
    </span>
  );
}

/** The title in the middle of an outline. The outline itself is `ShapeOutline`. */
function ShapeFace({ card, mark }: { card: LiveCanvasCard; mark: CanvasToneMark }) {
  return (
    <span className={`relative flex min-w-0 max-w-full flex-col items-center text-center ${NEAR} ${MOVES}`}>
      <span className="flex min-w-0 max-w-full items-center gap-1.5">
        {card.icon && <AppIcon name={card.icon} className={`size-3.5 shrink-0 ${INK}`} />}
        {card.tone && mark(card.tone)}
        <span className={`min-w-0 font-semibold [overflow-wrap:anywhere] ${TITLE_SIZE[card.size]} ${TITLE}`}>
          {card.title}
        </span>
      </span>
      {card.text && <ItemText text={card.text} className={`${TEXT} max-w-full [&_p]:text-center`} />}
      {card.picture && <ItemPicture picture={card.picture} isLoose />}
      {card.meta && <ItemMeta meta={card.meta} className="max-w-full" />}
      {card.media && <ItemMedia media={card.media} />}
      <Badges badges={card.badges} />
      {card.progress !== undefined && <ItemProgress value={card.progress} className="mt-1.5 h-[3px] w-14" />}
    </span>
  );
}

/**
 * The outline of a shape, drawn half a pixel inside its box: the outer edge of the
 * line stands on the box, as a CSS border does, so an arrow ends on it.
 */
function ShapeOutline({ card, box, isSelected }: { card: LiveCanvasCard; box: CanvasBox; isSelected: boolean }) {
  const { width, dash, isRound } = OUTLINE[card.border];
  const inset = Math.max(1, width) / 2;
  const w = Math.max(1, box.width - 2 * inset);
  const h = Math.max(1, box.height - 2 * inset);
  const outline = canvasShapePath(card.shape, w, h);
  const detail = canvasShapeDetail(card.shape, w, h);
  const line = {
    strokeWidth: width,
    strokeDasharray: dash,
    strokeLinecap: isRound ? ('round' as const) : undefined,
    strokeLinejoin: 'round' as const,
  };
  return (
    <svg
      aria-hidden="true"
      className="pointer-events-none absolute left-0 top-0 overflow-visible"
      width={box.width}
      height={box.height}
    >
      <g transform={`translate(${inset} ${inset})`}>
        <path
          d={outline}
          className={`fill-[var(--cv-fill)] ${width ? 'stroke-[var(--cv-edge)]' : ''}`}
          {...line}
        />
        {card.border === 'double' && (
          <path
            d={outline}
            className={`fill-none ${card.fill === 'none' ? 'stroke-ink' : 'stroke-[var(--cv-fill)]'}`}
            strokeWidth={1}
          />
        )}
        {detail && width > 0 && <path d={detail} className="fill-none stroke-[var(--cv-edge)]" {...line} />}
        {/* The press area has the form of the shape, and that clip cuts its focus outline. */}
        <path
          d={outline}
          className="hidden fill-none stroke-accent/60 group-has-[[data-canvas-select]:focus-visible]/item:block"
          strokeWidth={5}
          strokeLinejoin="round"
        />
        {isSelected && (
          <path d={outline} className="fill-none stroke-accent" strokeWidth={2} strokeLinejoin="round" />
        )}
      </g>
    </svg>
  );
}

/** A motion of the whole item. It changes no edge, so each arrow stays on the item. */
const MOTION = {
  pulse: 'animate-canvas-pulse',
  glow: 'animate-canvas-glow',
} as const;

/**
 * One item at its box. `data-canvas-card` is the body that the layout measures. A
 * frame is as large as its box, and the layout measures its header.
 */
function Item({
  card,
  box,
  mark,
  after,
  isSelected,
  onSelect,
  talkCount,
  talkState,
  isTalkOpen,
  onTalk,
}: {
  card: LiveCanvasCard;
  box: CanvasBox;
  mark: CanvasToneMark;
  /** The titles of the items that the arrows of this item come from, as one line. */
  after: string;
  isSelected: boolean;
  /** Absent on a board where a person selects nothing. */
  onSelect?: (itemId: string) => void;
  /** How many messages and saved drafts the threads of the item hold. */
  talkCount: number;
  /** Absent for an item with no thread. */
  talkState?: CanvasTalkState;
  isTalkOpen: boolean;
  /** Opens the discussion of the item. Absent when there is nothing to show or to write. */
  onTalk?: (itemId: string) => void;
}) {
  const { kind } = card;
  const isFrame = kind === 'frame';
  const isShape = kind === 'shape';
  const isBoxed = kind === 'card' || isFrame;
  const room = SHAPE_ROOM[card.shape];
  const style: CSSProperties = isFrame
    ? { height: box.height }
    : isShape
      ? {
          minHeight: canvasItemMinHeight(card),
          padding: `${Math.round(box.width * room.top)}px ${Math.round(box.width * room.x)}px ${Math.round(box.width * room.bottom)}px`,
        }
      : { minHeight: canvasItemMinHeight(card) };
  const ring =
    isSelected && !isShape ? 'outline outline-2 outline-offset-2 outline-accent' : '';
  const whole = card.animation === 'pulse' || card.animation === 'glow' ? card.animation : undefined;
  const motionName = whole === 'glow' && !isBoxed ? 'animate-canvas-glow-form' : whole && MOTION[whole];
  const motion = motionName ? `${motionName} motion-reduce:animate-none` : '';
  return (
    <div
      className="absolute"
      style={{ left: box.x, top: box.y, width: box.width, zIndex: box.depth }}
    >
      <div
        data-canvas-card={card.id}
        data-canvas-kind={kind}
        // A sticky note with no colour is yellow, as a paper one is.
        data-canvas-color={card.color ?? (kind === 'note' ? 'yellow' : undefined)}
        data-canvas-tone={card.tone}
        data-motion={whole ? undefined : card.animation}
        data-canvas-fill={card.fill}
        data-canvas-border={isBoxed ? card.border : undefined}
        className={`group/item relative text-left ${motion} ${ring} ${
          isBoxed ? 'border border-[var(--cv-edge)] bg-[var(--cv-fill)]' : ''
        } ${isShape ? 'flex items-center justify-center' : ''} ${
          kind === 'note' ? '[filter:drop-shadow(2px_2px_0_var(--cv-edge))]' : ''
        }`}
        style={style}
      >
        {isShape && <ShapeOutline card={card} box={box} isSelected={isSelected} />}
        {onSelect && (
          <button
            type="button"
            data-canvas-select={card.id}
            aria-pressed={isSelected}
            aria-label={card.title}
            className="absolute inset-0 cursor-pointer transition-colors duration-150 hover:bg-white/5 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent/60 motion-reduce:transition-none"
            style={
              isShape ? { clipPath: `path('${canvasShapePath(card.shape, box.width, box.height)}')` } : undefined
            }
            onClick={() => onSelect(card.id)}
          />
        )}
        {isShape && (
          <FarTitle
            card={card}
            mark={mark}
            place="pointer-events-none justify-center px-2"
          />
        )}
        {/* The camera sets the scale variables on the frame. The words do not use them,
            so they drop them here and a zoom does not style the words again. A value
            that the frame can also have, such as 1, does not stop that. */}
        <div className={`pointer-events-none [--cv-mark:initial] [--cv-weight:initial] ${isShape ? 'relative min-w-0 max-w-full' : ''}`}>
          {kind === 'card' && <CardFace card={card} mark={mark} />}
          {kind === 'note' && <NoteFace card={card} mark={mark} />}
          {kind === 'text' && <TextFace card={card} />}
          {isShape && <ShapeFace card={card} mark={mark} />}
          {isFrame && (
            <div data-canvas-head="" className="relative px-3 pb-2 pt-2.5">
              <span className={`block ${NEAR} ${MOVES}`}>
                <Heading card={card} mark={mark} titleClass={`${TITLE_SIZE[card.size]} ${INK}`} />
              </span>
              <FarTitle card={card} mark={mark} titleClass={INK} />
              {card.text && <ItemText text={card.text} className={`${HINT} ${NEAR}`} />}
              {(card.picture || card.media) && (
                <span className={`block max-w-80 ${NEAR}`}>
                  {card.picture && <ItemPicture picture={card.picture} isLoose />}
                  {card.media && <ItemMedia media={card.media} />}
                </span>
              )}
              {(card.badges.length > 0 || card.progress !== undefined) && (
                <span className={`block ${NEAR} ${MOVES}`}>
                  <Badges badges={card.badges} />
                  {card.progress !== undefined && <ItemProgress value={card.progress} className="mt-2 h-[3px]" />}
                </span>
              )}
            </div>
          )}
          {after && <span className="sr-only">After {after}</span>}
        </div>
        {onTalk && (
          <TalkMark
            title={card.title}
            count={talkCount}
            state={talkState}
            isOpen={isTalkOpen}
            onOpen={() => onTalk(card.id)}
            corner={isShape ? cornerOf(card, box) : undefined}
          />
        )}
      </div>
      {card.caption && (
        <div
          data-canvas-caption=""
          className={`pt-1.5 text-center text-meta text-dialog-hint [overflow-wrap:anywhere] ${NEAR}`}
        >
          {card.caption}
        </div>
      )}
    </div>
  );
}

/**
 * A pan, a zoom and a patch of another item draw this item again only when its own
 * card, its box or its state changed. The layout makes each box again, so the boxes
 * compare by their numbers.
 */
export const CanvasItem = memo(Item, (held, next) => {
  const a = held.box;
  const b = next.box;
  return (
    held.card === next.card &&
    a.x === b.x &&
    a.y === b.y &&
    a.width === b.width &&
    a.height === b.height &&
    a.depth === b.depth &&
    held.mark === next.mark &&
    held.after === next.after &&
    held.isSelected === next.isSelected &&
    held.onSelect === next.onSelect &&
    held.talkCount === next.talkCount &&
    held.talkState === next.talkState &&
    held.isTalkOpen === next.isTalkOpen &&
    held.onTalk === next.onTalk
  );
});
