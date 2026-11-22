/**
 * The items of a live `canvas`: a card, a shape, a sticky note, a comment, a frame
 * and a text. The layout gives each item its box, and the item draws its face in it.
 * The colours come from the `--cv-*` paints of the style sheet.
 */

import { useState, type CSSProperties, type ReactNode } from 'react';

import { pictureFacts, pictureSource } from '../lib/app-image';
import {
  canvasItemMinHeight,
  canvasItemRadius,
  canvasShapeDetail,
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
import { InlineMarkdown } from './ChatContent';
import { AppIcon, CircleCheckIcon, ImageIcon, PlayIcon, SpeakerIcon } from './icons';

/** The mark of a tone, with its word for a screen reader. */
export type CanvasToneMark = (tone: LiveTone, className?: string) => ReactNode;

const TITLE = 'text-[color:var(--cv-title)]';
const TEXT = 'text-[color:var(--cv-text)]';
const HINT = 'text-[color:var(--cv-hint)]';
const INK = 'text-[color:var(--cv-ink)]';
/** The part of an item that a far camera hides: only the title stays readable. */
const NEAR = 'group-data-[far=true]/canvas:invisible';

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

/** The first letters of the first two words of a name. */
function initials(name: string): string {
  return name
    .split(/\s+/)
    .filter(Boolean)
    .slice(0, 2)
    .map((word) => [...word][0]?.toUpperCase() ?? '')
    .join('');
}

/**
 * A picture. On the top edge of a box it fills the width under the round corners; a
 * loose picture, in an item with no box edge at the top, has its own border.
 */
function ItemPicture({
  picture,
  radius,
  isLoose = false,
}: {
  picture: LiveCanvasPicture;
  radius: number;
  isLoose?: boolean;
}) {
  const src = pictureSource(picture);
  const ratio =
    picture.width && picture.height
      ? Math.min(2.4, Math.max(1, picture.width / picture.height))
      : 16 / 9;
  const corners = { borderRadius: isLoose ? radius : `${radius}px ${radius}px 0 0` };
  const edge = isLoose ? 'mt-2 w-full border' : 'border-b';
  if (!src) {
    return (
      <span
        className={`flex items-center gap-1.5 ${edge} border-[var(--cv-edge)] px-3 py-2 text-left text-meta ${HINT}`}
        style={corners}
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
      style={{ ...corners, aspectRatio: ratio }}
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
        className={`flex shrink-0 items-center justify-center rounded-full border border-current transition-transform duration-150 group-hover/media:scale-110 motion-reduce:transition-none ${
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

/** How many comments an item has, at its corner. */
function CommentCount({ count }: { count: number }) {
  return (
    <span className="absolute -right-2 -top-2 z-10 flex h-4 min-w-4 items-center justify-center rounded-full bg-accent px-1 font-mono text-meta font-bold tabular-nums text-accent-foreground ring-2 ring-ink">
      {count}
      <span className="sr-only">{count === 1 ? ' comment' : ' comments'}</span>
    </span>
  );
}

function Badges({ badges }: { badges: string[] }) {
  if (badges.length === 0) return null;
  return (
    <span className="mt-2 flex flex-wrap gap-1">
      {badges.map((badge) => (
        <span
          key={badge}
          className={`rounded-[3px] border border-[var(--cv-edge)] px-1.5 font-mono text-meta ${HINT}`}
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
      className={`absolute inset-0 hidden items-center overflow-hidden group-data-[far=true]/canvas:flex ${isTight ? 'gap-2' : 'gap-3'} ${place}`}
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
      {card.meta && (
        <span className={`ml-auto shrink-0 pl-2 font-mono text-meta tabular-nums ${HINT}`}>
          {card.meta}
        </span>
      )}
    </span>
  );
}

function Words({ text, lines }: { text: string; lines: string }) {
  return (
    <span className={`mt-1 block text-ui ${lines} ${TEXT} [overflow-wrap:anywhere]`}>
      <InlineMarkdown>{text}</InlineMarkdown>
    </span>
  );
}

function CardFace({ card, mark, radius }: { card: LiveCanvasCard; mark: CanvasToneMark; radius: number }) {
  const inner = Math.max(0, radius - 1);
  return (
    <>
      {/* A picture reads at each distance, so a far camera keeps it. */}
      {card.picture && <ItemPicture picture={card.picture} radius={inner} />}
      <span className="relative block">
        <span className={`block ${PAD[card.size]} ${NEAR}`}>
          <Heading card={card} mark={mark} titleClass={`${TITLE_SIZE[card.size]} ${TITLE}`} />
          {card.text && <Words text={card.text} lines="line-clamp-4" />}
          {card.media && <ItemMedia media={card.media} />}
          <Badges badges={card.badges} />
        </span>
        <FarTitle card={card} mark={mark} />
      </span>
      {card.progress !== undefined && (
        <span
          className="absolute inset-x-0 bottom-0 block overflow-hidden"
          style={{ borderRadius: `0 0 ${inner}px ${inner}px` }}
        >
          <ItemProgress value={card.progress} className="h-[3px]" />
        </span>
      )}
    </>
  );
}

/** A sticky note: a sheet of the colour with a folded corner. */
function NoteFace({ card, mark, radius }: { card: LiveCanvasCard; mark: CanvasToneMark; radius: number }) {
  const fold = 14;
  return (
    <>
      <span
        aria-hidden="true"
        className="absolute inset-0 bg-[var(--cv-fill)]"
        style={{
          borderRadius: radius,
          clipPath: `polygon(0 0, 100% 0, 100% calc(100% - ${fold}px), calc(100% - ${fold}px) 100%, 0 100%)`,
        }}
      />
      <span
        aria-hidden="true"
        className="absolute bottom-0 right-0 bg-[color-mix(in_oklab,var(--cv-fill)_72%,var(--fg))]"
        style={{ width: fold, height: fold, clipPath: 'polygon(0 0, 100% 0, 0 100%)' }}
      />
      <span className={`relative block ${NEAR}`}>
        {card.picture && <ItemPicture picture={card.picture} radius={radius} />}
        <span className={`block ${PAD[card.size]} pb-4`}>
          <Heading card={card} mark={mark} titleClass={`${TITLE_SIZE[card.size]} ${TITLE}`} />
          {card.text && <Words text={card.text} lines="line-clamp-6" />}
          {card.media && <ItemMedia media={card.media} />}
          <Badges badges={card.badges} />
          {card.progress !== undefined && <ItemProgress value={card.progress} className="mt-2 h-[3px]" />}
        </span>
      </span>
      <FarTitle card={card} mark={mark} />
    </>
  );
}

/** A remark of a person: who wrote it, the words, and a check mark when it is done. */
function CommentFace({ card, mark }: { card: LiveCanvasCard; mark: CanvasToneMark }) {
  const author = card.author ?? card.title;
  return (
    <>
      <span
        aria-hidden="true"
        className="absolute -bottom-[5px] left-4 size-2 rotate-45 border-b border-r border-[var(--cv-edge)] bg-[var(--cv-fill)]"
      />
      <span className={`relative block ${PAD[card.size]} ${NEAR} ${card.is_resolved ? 'opacity-60' : ''}`}>
        <span className="flex min-w-0 items-center gap-1.5">
          <span
            aria-hidden="true"
            className="flex size-5 shrink-0 items-center justify-center rounded-full bg-[var(--cv-solid)] font-mono text-meta font-bold text-[color:var(--cv-on)]"
          >
            {initials(author)}
          </span>
          {card.tone && mark(card.tone)}
          <span className={`min-w-0 truncate text-ui font-semibold ${TITLE}`}>{author}</span>
          {card.author && <span className={`min-w-0 truncate text-meta ${HINT}`}>{card.title}</span>}
          {card.is_resolved ? (
            <span className="ml-auto flex shrink-0 items-center text-ok">
              <CircleCheckIcon className="size-3.5" />
              <span className="sr-only">Resolved</span>
            </span>
          ) : (
            card.meta && (
              <span className={`ml-auto shrink-0 pl-2 font-mono text-meta tabular-nums ${HINT}`}>{card.meta}</span>
            )
          )}
        </span>
        {card.text && <Words text={card.text} lines="line-clamp-6" />}
        {card.picture && <ItemPicture picture={card.picture} radius={4} isLoose />}
        {card.media && <ItemMedia media={card.media} />}
        <Badges badges={card.badges} />
      </span>
      <FarTitle card={card} mark={mark} />
    </>
  );
}

/** A heading on the board: words with no border. */
function TextFace({ card }: { card: LiveCanvasCard }) {
  return (
    <span className="block">
      <span className="flex min-w-0 items-center gap-2">
        {card.icon && <AppIcon name={card.icon} className={`size-4 shrink-0 ${INK}`} />}
        <span
          className={`min-w-0 [overflow-wrap:anywhere] ${HEADING_SIZE[card.size]} ${card.color ? INK : TITLE}`}
        >
          {card.title}
        </span>
      </span>
      {card.text && (
        <span className={`mt-0.5 block text-ui ${HINT} [overflow-wrap:anywhere] ${NEAR}`}>
          <InlineMarkdown>{card.text}</InlineMarkdown>
        </span>
      )}
      {(card.picture || card.media) && (
        <span className={`block ${NEAR}`}>
          {card.picture && <ItemPicture picture={card.picture} radius={6} isLoose />}
          {card.media && <ItemMedia media={card.media} />}
        </span>
      )}
    </span>
  );
}

/** The title in the middle of an outline. The outline itself is `ShapeOutline`. */
function ShapeFace({ card, mark }: { card: LiveCanvasCard; mark: CanvasToneMark }) {
  return (
    <span className={`relative flex min-w-0 max-w-full flex-col items-center text-center ${NEAR}`}>
      <span className="flex min-w-0 max-w-full items-center gap-1.5">
        {card.icon && <AppIcon name={card.icon} className={`size-3.5 shrink-0 ${INK}`} />}
        {card.tone && mark(card.tone)}
        <span className={`min-w-0 font-semibold [overflow-wrap:anywhere] ${TITLE_SIZE[card.size]} ${TITLE}`}>
          {card.title}
        </span>
      </span>
      {card.text && (
        <span className={`mt-0.5 line-clamp-3 block text-meta ${TEXT} [overflow-wrap:anywhere]`}>
          <InlineMarkdown>{card.text}</InlineMarkdown>
        </span>
      )}
      {card.picture && <ItemPicture picture={card.picture} radius={4} isLoose />}
      {card.media && <ItemMedia media={card.media} />}
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
        {isSelected && (
          <path d={outline} className="fill-none stroke-accent" strokeWidth={2} strokeLinejoin="round" />
        )}
      </g>
    </svg>
  );
}

const MOTION = {
  pulse: 'animate-canvas-pulse',
  glow: 'animate-canvas-glow',
  bounce: 'animate-canvas-bounce',
  shake: 'animate-canvas-shake',
} as const;

/**
 * One item at its box. `data-canvas-card` is the body that the layout measures. A
 * frame is as large as its box, and the layout measures its header.
 */
export function CanvasItem({
  card,
  box,
  mark,
  sources,
  isSelected,
  onSelect,
}: {
  card: LiveCanvasCard;
  box: CanvasBox;
  mark: CanvasToneMark;
  /** The titles of the items that the arrows of this item come from. */
  sources: string[];
  isSelected: boolean;
  /** Absent on a board where a person selects nothing. */
  onSelect?: () => void;
}) {
  const { kind } = card;
  const isFrame = kind === 'frame';
  const isShape = kind === 'shape';
  const isBoxed = kind === 'card' || kind === 'comment' || isFrame;
  const radius = canvasItemRadius(card);
  const room = SHAPE_ROOM[card.shape];
  const style: CSSProperties = isFrame
    ? { borderRadius: radius, height: box.height }
    : isShape
      ? {
          minHeight: canvasItemMinHeight(card),
          padding: `${Math.round(box.width * room.top)}px ${Math.round(box.width * room.x)}px ${Math.round(box.width * room.bottom)}px`,
        }
      : { borderRadius: radius, minHeight: canvasItemMinHeight(card) };
  const ring =
    isSelected && !isShape ? 'outline outline-2 outline-offset-2 outline-accent' : '';
  const motion = card.animation ? `${MOTION[card.animation]} motion-reduce:animate-none` : '';
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
        data-canvas-fill={card.fill}
        data-canvas-border={isBoxed ? card.border : undefined}
        className={`relative text-left ${motion} ${ring} ${
          isBoxed ? 'border border-[var(--cv-edge)] bg-[var(--cv-fill)]' : ''
        } ${isShape ? 'flex items-center justify-center' : ''} ${
          kind === 'note' ? '[filter:drop-shadow(0_1px_1.5px_rgb(0_0_0/0.3))]' : ''
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
              isShape
                ? { clipPath: `path('${canvasShapePath(card.shape, box.width, box.height)}')` }
                : { borderRadius: Math.max(0, radius - 1) }
            }
            onClick={onSelect}
          />
        )}
        {isShape && (
          <FarTitle
            card={card}
            mark={mark}
            place="pointer-events-none justify-center px-2"
          />
        )}
        <div className={`pointer-events-none ${isShape ? 'relative min-w-0 max-w-full' : ''}`}>
          {kind === 'card' && <CardFace card={card} mark={mark} radius={radius} />}
          {kind === 'note' && <NoteFace card={card} mark={mark} radius={radius} />}
          {kind === 'comment' && <CommentFace card={card} mark={mark} />}
          {kind === 'text' && <TextFace card={card} />}
          {isShape && <ShapeFace card={card} mark={mark} />}
          {isFrame && (
            <div data-canvas-head="" className="relative px-3 pb-2 pt-2.5">
              <span className={`block ${NEAR}`}>
                <Heading card={card} mark={mark} titleClass={`${TITLE_SIZE[card.size]} ${INK}`} />
              </span>
              <FarTitle card={card} mark={mark} titleClass={INK} />
              {card.text && (
                <span className={`mt-0.5 block truncate text-meta ${HINT} ${NEAR}`}>
                  <InlineMarkdown>{card.text}</InlineMarkdown>
                </span>
              )}
              {(card.picture || card.media) && (
                <span className={`block max-w-80 ${NEAR}`}>
                  {card.picture && <ItemPicture picture={card.picture} radius={4} isLoose />}
                  {card.media && <ItemMedia media={card.media} />}
                </span>
              )}
            </div>
          )}
          {sources.length > 0 && <span className="sr-only">After {sources.join(', ')}</span>}
        </div>
        {card.comments !== undefined && <CommentCount count={card.comments} />}
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
