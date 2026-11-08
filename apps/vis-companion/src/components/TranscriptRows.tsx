import type { ComponentProps } from 'react';
import { AssistantMessage, UserMessage } from './ChatContent';
import type { TranscriptTurn } from '../lib/types';

/**
 * The scroller of a transcript. The session screen and the HTML export share it, so an exported
 * transcript lays out at the same width, with the same container queries, as the screen.
 *
 * iOS WebKit reads `overscroll-behavior: contain` as no rubber band at all: measured on an
 * iOS 18.6 simulator, the transcript stopped dead at its ends. Only iOS supports
 * `-webkit-touch-callout`, so iOS gets its own bounce back and other hosts keep `contain`.
 */
export const TRANSCRIPT_SCROLLER_CLASS =
  '@container/transcript min-h-0 flex-1 overflow-x-hidden overflow-y-auto overscroll-contain supports-[-webkit-touch-callout:none]:overscroll-y-auto scroll-pb-8 [overflow-anchor:none]';

/**
 * The reading column inside the scroller. `outline` keeps the gutter of the outline rail
 * beside the text, and `empty` centres the new-session cue instead of the turns.
 */
export function transcriptColumnClass({ zen, outline, empty }: { zen: boolean; outline: boolean; empty: boolean }): string {
  return `transcript-column mx-auto min-h-full w-full pt-4 sm:pt-6 ${zen ? 'zen-column' : 'max-w-3xl mouse:max-w-6xl'} ${outline ? 'has-outline' : ''} ${
    empty ? 'flex flex-col pb-4 sm:pb-6' : 'flex flex-col justify-end pb-10'
  }`;
}

/** The transcript shows its outline rail, and keeps the rail's gutter, from two turns. */
export const OUTLINE_MIN_TURNS = 2;

/**
 * The layer of the outline rail over the transcript. The rail stands in the gutter that
 * `has-outline` keeps beside the text, centred on the transcript, and never in the unsafe edge
 * of a notched phone. A narrow screen has no gutter: the rail stands over the text, hidden.
 */
export const OUTLINE_RAIL_CLASS =
  'pointer-events-none absolute inset-0 z-10 mx-auto flex max-w-3xl items-center justify-end pr-[env(safe-area-inset-right)] mouse:max-w-6xl';

/**
 * One persisted turn of a transcript: the request, then the answer. `hidesRequest` drops the
 * request of a wake that a subagent of this session sent. The HTML export draws the same row.
 */
export function StoredTurnRow({
  turn,
  isFirst,
  hidesRequest = false,
  ...answer
}: Omit<ComponentProps<typeof AssistantMessage>, 'turn'> & {
  turn: TranscriptTurn;
  isFirst: boolean;
  hidesRequest?: boolean;
}) {
  const request = turn.request ?? '';
  // A turn skips its own paint, in `AssistantMessage` (`useMeasuredPaintSkip`), never from
  // this wrapper: the size a skip stands in for has to be the one that turn MEASURED.
  return (
    <div className={isFirst ? '' : 'mt-10'} data-turn-id={turn.turn_id}>
      {(request || (turn.attachments?.length ?? 0) > 0) && !hidesRequest && (
        <UserMessage
          position={turn.position}
          createdAt={turn.created_at}
          requestKind={turn.request_kind}
          council={turn.council}
          attachments={turn.attachments}
        >
          {request}
        </UserMessage>
      )}
      <AssistantMessage turn={turn} {...answer} />
    </div>
  );
}
