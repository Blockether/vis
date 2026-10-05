/**
 * The transcript outline: one row for each turn of a session, oldest first.
 *
 * A long session hides its own beginning. To find the third prompt back, the reader
 * has to scroll past every answer after it. The outline names each turn by the words
 * that opened it and marks the turn on screen, so the reader can go straight back to
 * it (Blockether/vis#316).
 */
import { EDGE_MARGIN, type Viewport } from './anchored-menu';
import { parseUserMessage } from './paste';
import { isAtBottom } from './reading-position';
import type { ContentBlock, CouncilRequest, RequestKind } from './types';

/** One turn in the outline. */
export interface OutlineEntry {
  /** The turn's id, as the transcript wears it in `data-turn-id`. */
  id: string;
  /** The words that opened the turn, on one line. */
  label: string;
  /** The start of the turn's answer as plain text on one line, or `''` without an answer. */
  answer: string;
  /** The state of the turn's answer, or `null` when the turn does not give one. */
  status: OutlineStatus | null;
  /** The turn answered a Council message, not a prompt from the reader. */
  isCouncil: boolean;
}

/** The state of a turn's answer, as the outline preview names it. */
export type OutlineStatus = 'done' | 'running' | 'queued' | 'waiting' | 'cancelled' | 'failed';

/** The most lines the rail paints. In a longer session, turns share a line. */
export const RAIL_MAX_LINES = 12;

/** Where a jump puts the top of a turn, below the top edge of the scroller. */
export const JUMP_MARGIN = 16;

/**
 * How far below the top edge of the scroller a turn must reach to be the turn on
 * screen. It is more than `JUMP_MARGIN`, so a turn that a jump put at the top counts.
 */
const READING_INSET = 32;

/** The longest label that is kept. The row truncates long before this. */
const LABEL_MAX = 160;

/**
 * The longest paste, in characters, that a label reads as its words. A longer paste,
 * such as a log or a file, reads as its summary: `[Pasted #1: 60 lines, 1.1KB]`.
 */
const SHORT_PASTE_MAX = 500;

/** The space between the rail and the card that it opens. */
const RAIL_GAP = 8;

/** The width of the outline card, in pixels. A narrow screen paints it narrower. */
export const OUTLINE_WIDTH = 288;

/** The tallest that the outline card grows, in pixels. A longer outline scrolls in it. */
const CARD_MAX_HEIGHT = 420;

/** Where the outline card stands, the width that it paints and the height that it can use. */
export type OutlinePlace = { left: number; top: number; width: number; maxHeight: number };

/** The token that the composer puts in a prompt for each image, such as `[IMAGE #1]`. */
const IMAGE_TOKEN = /\[IMAGE #\d+\]/i;

/** The longest answer excerpt that is kept. The preview clamps it to a few lines. */
const ANSWER_MAX = 400;

/**
 * The longest part of an answer that the excerpt reads. Markdown marks make the
 * source longer than its words, so it is longer than `ANSWER_MAX`.
 */
const ANSWER_SOURCE_MAX = 4 * ANSWER_MAX;

/** A line that opens or closes a fenced code block. */
const FENCE_LINE = /^\s{0,3}(`{3,}|~{3,})/;

/** A line that only divides: a thematic break, a heading rule or the rule of a table. */
const DIVIDER_LINE = /^(?:[\s|:=-]*[-=][\s|:=-]*|\s*(?:[*_]\s*){3,})$/;

/** The marks that open a Markdown line: quotes, headings, list items and task boxes. */
const LINE_MARKS = /^(?:\s*(?:>|#{1,6}(?=\s)|[-*+](?=\s)|\d{1,9}[.)](?=\s)|\[[ xX]\](?=\s)))+\s*/;

/**
 * The marks inside a Markdown line. A code span keeps its code, a link or an image
 * keeps its text, an autolink keeps its address and emphasis keeps its words.
 */
const INLINE_MARKS =
  /(`+)(.*?)\1|!?\[([^\]]*)\]\([^)]*\)|<((?:https?|mailto):[^>\s]*)>|\*(?=\S)([^*]*?\S)\*(?!\w)|\*\*|__|~~/g;

/** A character that a backslash keeps from being a Markdown mark. */
const ESCAPED = /\\([\\`*_{}[\]()#+.!|>~-])/g;

/**
 * An escaped character waits in the private use area while the marks go, so that no
 * mark pattern can read it. `KEPT_MARK` finds it again.
 */
const KEPT = 0xe000;
const KEPT_MARK = /[\ue000-\ue07f]/g;

/**
 * The words that opened a turn, on one line. A short paste counts as its words, and a
 * longer paste counts as its summary. An image has no words: its token and its caption go.
 */
export function promptLabel(request: string | undefined, attachments = 0): string {
  const parts = parseUserMessage(request ?? '');
  const words = parts
    .map((part) =>
      part.type === 'text'
        ? part.text.split(IMAGE_TOKEN).join(' ')
        : part.type === 'paste'
          ? part.content.length > SHORT_PASTE_MAX
            ? part.summary
            : part.content
          : '',
    )
    .join(' ')
    .replace(/\s+/g, ' ')
    .trim();
  if (words) return words.length > LABEL_MAX ? `${words.slice(0, LABEL_MAX - 1)}…` : words;
  const pictures = parts.some((part) => part.type === 'image') || IMAGE_TOKEN.test(request ?? '');
  return attachments > 0 || pictures ? 'Attachments only' : 'Empty message';
}

/**
 * The start of an answer as plain text on one line. Code blocks and dividers go, and
 * so do the marks of Markdown. The words of links, code spans and emphasis stay.
 */
export function answerExcerpt(markdown: string): string {
  const lines: string[] = [];
  let fence = '';
  for (const line of markdown.split('\n')) {
    const marker = FENCE_LINE.exec(line)?.[1];
    if (fence) {
      // Only a fence of the same character, at least as long, closes the block.
      if (marker && marker[0] === fence[0] && marker.length >= fence.length) fence = '';
    } else if (marker) {
      fence = marker;
    } else if (!DIVIDER_LINE.test(line)) {
      // The cells of a table row stay as words.
      const words = line.trimStart().startsWith('|') ? line.replace(/\|/g, ' ') : line;
      lines.push(words.replace(LINE_MARKS, ''));
    }
  }
  const text = lines
    .join(' ')
    .replace(ESCAPED, (_escape, mark: string) => String.fromCharCode(KEPT + mark.charCodeAt(0)))
    .replace(
      INLINE_MARKS,
      (_mark, _ticks, code?: string, label?: string, link?: string, stressed?: string) =>
        code ?? label ?? link ?? stressed ?? '',
    )
    .replace(KEPT_MARK, (kept) => String.fromCharCode(kept.charCodeAt(0) - KEPT))
    .replace(/\s+/g, ' ')
    .trim();
  return text.length > ANSWER_MAX ? `${text.slice(0, ANSWER_MAX - 1)}…` : text;
}

/** The answer of a turn as Markdown: its prose and its spoken text, in order. */
function answerMarkdown(content: readonly ContentBlock[]): string {
  const parts: string[] = [];
  let length = 0;
  for (const block of content) {
    if (length >= ANSWER_SOURCE_MAX) break;
    const text = (
      block.type === 'prose' ? block.markdown : block.type === 'speech' ? block.text : ''
    )?.trim();
    if (!text) continue;
    parts.push(text);
    length += text.length;
  }
  return parts.join('\n\n');
}

/**
 * Each status that the gateway gives a turn, as its outline state. The transcript sends the
 * durable `done`, `running`, `error` or `interrupted`. The fork points send the wire form:
 * `completed`, `streaming`, `failed` or `cancelled`. A turn that waits for a person is
 * `suspended`.
 */
const STATUSES = new Map<string, OutlineStatus>([
  ['done', 'done'],
  ['completed', 'done'],
  ['running', 'running'],
  ['streaming', 'running'],
  ['queued', 'queued'],
  ['pending', 'queued'],
  ['suspended', 'waiting'],
  ['cancelled', 'cancelled'],
  ['interrupted', 'cancelled'],
  ['failed', 'failed'],
  ['error', 'failed'],
]);

/**
 * The outline state of a turn's wire `status`, or `null` for a status that the outline
 * does not name. As in the transcript, a settled turn whose loop ended in a cancel is
 * cancelled.
 */
export function outlineStatus(status?: string, priorOutcome?: string): OutlineStatus | null {
  const state = STATUSES.get(status ?? '') ?? null;
  return priorOutcome === 'cancelled' && state !== 'running' && state !== 'queued' ? 'cancelled' : state;
}

/**
 * The outline row of one turn. A transcript row gives its content, and a gateway fork
 * point gives the start of its answer.
 */
export function outlineEntry(turn: {
  turn_id: string;
  request?: string;
  request_kind?: RequestKind;
  council?: CouncilRequest;
  attachments?: readonly unknown[];
  answer?: string;
  content?: readonly ContentBlock[];
  status?: string;
  prior_outcome?: string;
}): OutlineEntry {
  const answer = turn.content ? answerMarkdown(turn.content) : (turn.answer ?? '');
  return {
    id: turn.turn_id,
    label: promptLabel(turn.request || turn.council?.content, turn.attachments?.length ?? 0),
    answer: answerExcerpt(answer.slice(0, ANSWER_SOURCE_MAX)),
    status: outlineStatus(turn.status, turn.prior_outcome),
    isCouncil: turn.request_kind === 'council',
  };
}

/**
 * The outline of the whole session: the list from the gateway, then each held turn
 * that the list does not name. Such a turn started after the gateway read its list.
 * A listed turn takes the state of its held row, which follows the live turn. It takes
 * the answer of that row when the list has none.
 */
export function mergeOutline(
  listed: readonly OutlineEntry[],
  held: readonly OutlineEntry[],
): OutlineEntry[] {
  const rows = new Map(held.map((entry) => [entry.id, entry]));
  const merged = listed.map((entry) => {
    const row = rows.get(entry.id);
    if (!row) return entry;
    const answer = entry.answer || row.answer;
    const status = row.status ?? entry.status;
    return answer === entry.answer && status === entry.status ? entry : { ...entry, answer, status };
  });
  const known = new Set(listed.map((entry) => entry.id));
  return [...merged, ...held.filter((entry) => !known.has(entry.id))];
}

/**
 * The rail for a session of `total` turns: the number of lines, and the line that
 * holds turn `current` (`-1` for none). Each line is one turn until the session has
 * more than `max` turns. Then the turns share the lines in equal parts.
 */
export function railLines(
  total: number,
  current: number,
  max = RAIL_MAX_LINES,
): { count: number; active: number } {
  const count = Math.max(0, Math.min(total, max));
  if (count === 0 || current < 0 || current >= total) return { count, active: -1 };
  return { count, active: Math.min(count - 1, Math.floor((current * count) / total)) };
}

/**
 * The id of the turn on screen: the first turn that reaches below the top edge of
 * the scroller, or the last turn when the end is on screen. The turns are the direct
 * children of `column` that carry `data-turn-id`.
 */
export function readingTurnId(viewport: HTMLElement, column: HTMLElement | null): string | null {
  const rows = column
    ? Array.from(column.querySelectorAll<HTMLElement>(':scope > [data-turn-id]'))
    : [];
  if (rows.length === 0) return null;
  if (isAtBottom(viewport)) return rows[rows.length - 1].dataset.turnId ?? null;
  const line = viewport.getBoundingClientRect().top + READING_INSET;
  // The rows stand in document order, so their bottom edges only grow.
  let low = 0;
  let high = rows.length - 1;
  while (low < high) {
    const middle = (low + high) >> 1;
    if (rows[middle].getBoundingClientRect().bottom > line) high = middle;
    else low = middle + 1;
  }
  return rows[low].dataset.turnId ?? null;
}

/**
 * Where the outline card stands: left of the rail, centred on it, inside the
 * screen. `height` is the natural height of the card, or an estimate before the
 * card is measured. The card never grows past the cap that it paints.
 */
export function outlinePanelPosition(
  rail: { top: number; bottom: number; left: number },
  width: number,
  height: number,
  viewport: Viewport,
): OutlinePlace {
  const painted = viewport.width <= 0 ? width : Math.min(width, viewport.width - 2 * EDGE_MARGIN);
  const rightmost = viewport.width <= 0 ? Infinity : viewport.width - EDGE_MARGIN - painted;
  const left = Math.round(
    Math.max(EDGE_MARGIN, Math.min(rail.left - RAIL_GAP - painted, rightmost)),
  );
  if (viewport.height <= 0) {
    return { left, top: Math.round(rail.top), width: painted, maxHeight: CARD_MAX_HEIGHT };
  }
  const maxHeight = Math.min(CARD_MAX_HEIGHT, viewport.height - 2 * EDGE_MARGIN);
  const tall = Math.min(height, maxHeight);
  const lowest = viewport.height - EDGE_MARGIN - tall;
  const centred = (rail.top + rail.bottom) / 2 - tall / 2;
  const top = Math.round(Math.max(EDGE_MARGIN, Math.min(centred, lowest)));
  return { left, top, width: painted, maxHeight };
}

/** The width of the answer preview, in pixels. A narrow screen paints it narrower. */
export const PREVIEW_WIDTH = 320;

/** The narrowest preview that is worth its place. With less room, no preview shows. */
const PREVIEW_MIN_WIDTH = 200;

/** The box that a preview describes: a row of the card, or a finger on the rail. */
export type PreviewAnchor = { left: number; top: number; bottom: number };

/** Where the answer preview stands, and the width that it paints. */
export type PreviewPlace = { left: number; top: number; width: number };

/**
 * Where the answer preview stands: left of its anchor, centred on it, inside the
 * screen. `height` is the measured height of the preview. The result is `null` when
 * the screen has no room for the preview left of the anchor.
 */
export function previewPosition(
  anchor: PreviewAnchor,
  height: number,
  viewport: Viewport,
): PreviewPlace | null {
  const width = Math.floor(Math.min(PREVIEW_WIDTH, anchor.left - RAIL_GAP - EDGE_MARGIN));
  if (width < PREVIEW_MIN_WIDTH) return null;
  const centred = (anchor.top + anchor.bottom) / 2 - height / 2;
  const lowest = viewport.height <= 0 ? centred : viewport.height - EDGE_MARGIN - height;
  return {
    left: Math.round(anchor.left - RAIL_GAP - width),
    top: Math.round(Math.max(EDGE_MARGIN, Math.min(centred, lowest))),
    width,
  };
}

/** The shortest finger travel that moves a scrub on the rail by one turn, in pixels. */
const SCRUB_STEP_MIN = 6;

/** The longest finger travel that moves a scrub on the rail by one turn, in pixels. */
const SCRUB_STEP_MAX = 32;

/**
 * The finger travel that moves a scrub on the rail by one turn. `reach` is the travel
 * that crosses the whole session. A finger can hit each step of a short session, and
 * it can still tell the steps of a long session apart.
 */
export function scrubStep(reach: number, total: number): number {
  return Math.min(SCRUB_STEP_MAX, Math.max(SCRUB_STEP_MIN, reach / Math.max(1, total)));
}

/**
 * The turn that a scrub names, as an index into a list of `count` turns. The scrub
 * starts at `start` and moves one turn for each `step` of `travel`: up to older
 * turns, down to newer turns. It stops at the ends of the list.
 */
export function scrubIndex(start: number, travel: number, step: number, count: number): number {
  if (count <= 0) return -1;
  return Math.max(0, Math.min(count - 1, start + Math.round(travel / step)));
}
