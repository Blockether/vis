import { useRef, useState } from 'react';
import {
  setZenPicture,
  useZenPicture,
  zenPictureFromFile,
  type ZenSide,
} from '../lib/transcript-display';
import { ImageIcon, TrashIcon } from './icons';
import { Button, IconButton, Text } from './ui';

/**
 * The Zen pictures, one in each margin beside the transcript column. They are decoration:
 * hidden from assistive technology, never a hit target, and absent where the margins are narrow.
 */
export function ZenPictures() {
  const left = useZenPicture('left');
  const right = useZenPicture('right');
  if (!left && !right) return null;
  return (
    <div aria-hidden className="pointer-events-none absolute inset-0 z-0 overflow-hidden">
      {left && <img src={left} alt="" draggable={false} className="zen-picture zen-picture-left" />}
      {right && (
        <img src={right} alt="" draggable={false} className="zen-picture zen-picture-right" />
      )}
    </div>
  );
}

/** Choose, change or remove the Zen picture of one side. */
function ZenPictureSide({
  side,
  onProblem,
}: {
  side: ZenSide;
  onProblem: (problem: string | null) => void;
}) {
  const picture = useZenPicture(side);
  const input = useRef<HTMLInputElement>(null);
  const choose = async (file: File) => {
    try {
      const kept = setZenPicture(side, await zenPictureFromFile(file));
      onProblem(kept ? null : 'This device could not keep the picture. It shows until a reload.');
    } catch {
      onProblem('This browser cannot open that file as a picture.');
    }
  };
  return (
    <div className="flex min-w-0 items-center gap-3">
      <div className="grid size-12 shrink-0 place-items-center overflow-hidden border border-dialog-edge bg-ink">
        {picture ? (
          <img src={picture} alt={`Zen picture, ${side}`} className="size-full object-cover" />
        ) : (
          <ImageIcon className="size-4 text-dialog-hint" />
        )}
      </div>
      <Button variant="secondary" density="compact" onClick={() => input.current?.click()}>
        {picture ? `Change ${side}` : `Choose ${side}`}
      </Button>
      {picture && (
        <IconButton
          label={`Remove the ${side} Zen picture`}
          variant="remove"
          onClick={() => {
            setZenPicture(side, null);
            onProblem(null);
          }}
        >
          <TrashIcon className="size-3.5" />
        </IconButton>
      )}
      <input
        ref={input}
        type="file"
        accept="image/*"
        hidden
        onChange={(event) => {
          const file = event.currentTarget.files?.[0];
          // Clear the input, so the same file can be chosen again after a removal.
          event.currentTarget.value = '';
          if (file) void choose(file);
        }}
      />
    </div>
  );
}

/** The settings control for both Zen pictures. */
export function ZenPictureChoice() {
  const [problem, setProblem] = useState<string | null>(null);
  return (
    <div className="space-y-2">
      <div className="flex flex-wrap gap-x-6 gap-y-2">
        <ZenPictureSide side="left" onProblem={setProblem} />
        <ZenPictureSide side="right" onProblem={setProblem} />
      </div>
      {problem && (
        <Text as="p" variant="description" role="alert">
          {problem}
        </Text>
      )}
    </div>
  );
}
