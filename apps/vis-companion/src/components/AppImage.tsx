/**
 * The `image` node of an app. One picture fits the width and keeps its place
 * while it loads. With `before`, the node compares the old picture with the new
 * one, side by side or with a swipe line over both.
 */

import { useMemo, useState, type CSSProperties } from 'react';

import {
  IMAGE_COMPARE_MODES,
  IMAGE_COMPARE_WORDS,
  pictureFacts,
  pictureRatio,
  pictureSource,
  swipePosition,
  swipeRatio,
  type ImageCompareMode,
} from '../lib/app-image';
import type { LiveImageNode, LivePicture } from '../lib/live-view';
import { ExpandableImage } from './ImageViewer';
import { ImageIcon, SideBySideIcon, SwipeHandleIcon, SwipeIcon } from './icons';
import { Chip } from './ui';

/** No picture grows past this part of the screen height. */
const MAX_HEIGHT = '70vh';

/** The box of a picture of known size: as wide as the space, the picture or the height allows. */
function pictureBox(picture: LivePicture): CSSProperties | undefined {
  const ratio = pictureRatio(picture);
  if (!ratio || !picture.width || !picture.height) return undefined;
  return {
    aspectRatio: ratio,
    width: `min(100%, ${picture.width}px, calc(${MAX_HEIGHT} * ${picture.width} / ${picture.height}))`,
  };
}

const PICTURE = 'block h-full w-full object-contain';
const PLATE = 'max-w-full overflow-hidden border border-dialog-edge bg-panel-2';
const CAPTION = 'font-mono text-meta font-bold uppercase tracking-wide text-dialog-hint';

/**
 * One picture that opens full screen on press. Data that the browser cannot
 * decode shows the facts of the picture, not an empty plate.
 */
function Picture({ picture, alt }: { picture: LivePicture; alt: string }) {
  const src = useMemo(() => pictureSource(picture), [picture]);
  const [failed, setFailed] = useState<string | null>(null);
  const box = pictureBox(picture);
  if (!src) return <PictureFacts picture={picture} alt={alt} />;
  if (failed === src) return <PictureFacts picture={picture} alt={alt} isBroken />;
  return (
    <div className={PLATE} style={box ?? { maxHeight: MAX_HEIGHT }}>
      <ExpandableImage
        src={src}
        alt={alt}
        frameClassName="h-full w-full"
        className={box ? PICTURE : 'block h-auto max-h-[70vh] w-full object-contain'}
        onError={() => setFailed(src)}
      />
    </div>
  );
}

/** A picture without a preview, left out by the host or broken: its facts in the box it would take. */
function PictureFacts({ picture, alt, isBroken = false }: { picture: LivePicture; alt: string; isBroken?: boolean }) {
  const box = pictureBox(picture);
  const state = isBroken ? 'the preview could not be decoded' : 'no preview';
  return (
    <div
      role="img"
      aria-label={`${alt}, ${pictureFacts(picture)}, ${state}`}
      className={`${PLATE} grid min-h-16 place-items-center p-3`}
      style={box}
    >
      <span aria-hidden className="flex flex-col items-center gap-1 text-center font-mono text-meta text-dialog-hint">
        <ImageIcon className="size-4" />
        <span>{pictureFacts(picture)}</span>
        <span className="text-white">{alt}</span>
        {isBroken && <span className="text-err-ink">The preview could not be decoded</span>}
      </span>
    </div>
  );
}

/** The old and the new picture next to each other, each under its label. */
function SideBySide({ node, before }: { node: LiveImageNode; before: LivePicture }) {
  return (
    <div className="grid grid-cols-2 items-start gap-2">
      <figure className="flex min-w-0 flex-col gap-1">
        <figcaption className={CAPTION}>Before</figcaption>
        <Picture picture={before} alt={`Before: ${node.alt}`} />
      </figure>
      <figure className="flex min-w-0 flex-col gap-1">
        <figcaption className={CAPTION}>After</figcaption>
        <Picture picture={node} alt={`After: ${node.alt}`} />
      </figure>
    </div>
  );
}

/**
 * Both pictures on top of each other, the old one cut at the swipe line. The
 * slider covers the whole box, so a drag anywhere moves the line, and the arrow
 * keys move it too.
 */
function Swipe({ node, before }: { node: LiveImageNode; before: LivePicture }) {
  const [position, setPosition] = useState(50);
  const afterSrc = useMemo(() => pictureSource(node), [node]);
  const beforeSrc = useMemo(() => pictureSource(before), [before]);
  const pair = `${afterSrc}|${beforeSrc}`;
  const [failed, setFailed] = useState<string | null>(null);
  // Side by side shows which picture is missing or broken.
  if (!afterSrc || !beforeSrc || failed === pair) return <SideBySide node={node} before={before} />;
  const ratio = swipeRatio(node, before);
  const sized = ratio ? pictureBox(node.width && node.height ? node : before) : undefined;
  return (
    <div className={`relative ${PLATE} select-none`} style={sized}>
      <img
        src={afterSrc}
        alt={`After: ${node.alt}`}
        className={sized ? PICTURE : 'block h-auto max-h-[70vh] w-full object-contain'}
        onError={() => setFailed(pair)}
      />
      <img
        src={beforeSrc}
        onError={() => setFailed(pair)}
        alt={`Before: ${node.alt}`}
        className="pointer-events-none absolute inset-0 h-full w-full object-contain"
        style={{ clipPath: `inset(0 ${100 - position}% 0 0)` }}
      />
      <span aria-hidden className={`pointer-events-none absolute left-2 top-2 bg-panel/85 px-1.5 py-0.5 ${CAPTION}`}>
        Before
      </span>
      <span aria-hidden className={`pointer-events-none absolute right-2 top-2 bg-panel/85 px-1.5 py-0.5 ${CAPTION}`}>
        After
      </span>
      <input
        type="range"
        min={0}
        max={100}
        step={1}
        value={position}
        aria-label={`Swipe line of ${node.alt}`}
        aria-valuetext={`${position}% before`}
        onChange={(event) => setPosition(swipePosition(Number(event.target.value)))}
        className="app-focus-peer absolute inset-0 m-0 h-full w-full cursor-ew-resize appearance-none opacity-0"
      />
      <span
        aria-hidden
        className="pointer-events-none absolute inset-y-0 w-0.5 -translate-x-1/2 bg-accent"
        style={{ left: `${position}%` }}
      />
      <span
        aria-hidden
        className="pointer-events-none absolute top-1/2 grid size-11 -translate-x-1/2 -translate-y-1/2 place-items-center border-2 border-accent bg-panel text-accent-ink shadow app-focus-ring"
        style={{ left: `${position}%` }}
      >
        <SwipeHandleIcon className="size-4" />
      </span>
    </div>
  );
}

/** How a comparison shows; the reader picks. */
function CompareModes({
  mode,
  onMode,
  label,
}: {
  mode: ImageCompareMode;
  onMode: (mode: ImageCompareMode) => void;
  label: string;
}) {
  return (
    <div role="group" aria-label={label} className="flex gap-1.5">
      {IMAGE_COMPARE_MODES.map((one) => (
        <Chip key={one} isOn={one === mode} onClick={() => onMode(one)}>
          {one === 'side' ? <SideBySideIcon className="size-3.5" /> : <SwipeIcon className="size-3.5" />}
          {IMAGE_COMPARE_WORDS[one]}
        </Chip>
      ))}
    </div>
  );
}

/** The old picture against the new one. */
function PictureCompare({ node, before }: { node: LiveImageNode; before: LivePicture }) {
  const [mode, setMode] = useState<ImageCompareMode>('side');
  // The cell prints the label above; the modes stand at the start of the line under it.
  return (
    <div className="flex flex-col gap-2" role="group" aria-label={`Compare ${node.alt}`}>
      <CompareModes mode={mode} onMode={setMode} label="Compare mode" />
      {mode === 'side' ? <SideBySide node={node} before={before} /> : <Swipe node={node} before={before} />}
    </div>
  );
}

/** One picture, or the comparison of the old and the new picture. */
export function ImageNodeView({ node }: { node: LiveImageNode }) {
  if (node.before) return <PictureCompare node={node} before={node.before} />;
  return <Picture picture={node} alt={node.alt} />;
}
